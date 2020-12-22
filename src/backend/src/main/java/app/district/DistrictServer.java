
package app.district;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.HashSet;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.ConfigReader;
import app.DistrictData;

/*
* Public notifications:
* 1: <distrito>_no-users_x_y
* 2: <distrito>_too-many_x_y
* 3: <distrito>_less-users_x_y
* 4: <distrito>_infection-occured
* Private notifications:
* 1: <user>_got-contact
*/

public class DistrictServer {

    // --------------------------------------------------------------------------------------------------------------

    static String local;
    static ConfigReader config = new ConfigReader();
    static int NUM_WORKERS = Integer.parseInt(config.getWorkers());

    // --------------------------------------------------------------------------------------------------------------

    static int MAP_SIZE = config.getMapSize();
    static int LIMIT_USERS = config.getLimitUsers();

    // shared global map with shared lock
    static ReentrantLock GLOBAL_MAP_LOCK = new ReentrantLock();
    static UsersInPosition[][] GLOBAL_MATRIX = new UsersInPosition[MAP_SIZE][MAP_SIZE];
    static ConcurrentHashMap<User, HashSet<String>> GLOBAL_USER_POSITION = new ConcurrentHashMap<>();
    static TreeSet<Record> top5Positions = new TreeSet<>();

    // --------------------------------------------------------------------------------------------------------------

    public static void init() {
        for (int i = 0; i < MAP_SIZE; i++) {
            for (int j = 0; j < MAP_SIZE; j++) {
                GLOBAL_MATRIX[i][j] = new UsersInPosition();
            }
        }
    }

    public static DistrictData requestDistrictData(ZMQ.Socket socket_req) {

        // request to central server port
        String port = config.getPort("ports", "CENTRAL_SERVER_REP");
        boolean connected = socket_req.connect("tcp://*:" + port);

        System.out.println(
                "[District:" + local + "] " + (connected ? "connected" : "erro_connecting") + " to central server...");

        // error connecting, quit
        if (!connected)
            return null;

        // create request message
        String message = "district_" + local;
        socket_req.send(message);
        byte[] msg_recv = socket_req.recv();
        String msg = new String(msg_recv);

        // error handling not defined in CentralServer...(fixme)
        String[] campos = msg.split("_");
        if (campos[1].equals("ok")) {
            int router_port = Integer.parseInt(campos[2]);
            int pub_port = Integer.parseInt(campos[3]);
            return new DistrictData(router_port, pub_port);
        } else
            return null;
    }

    public static void main(String[] args) {

        init();

        local = args[0];

        System.out.println("[District:" + local + "] Server started...");

        try (ZContext context = new ZContext();
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket socket_router = context.createSocket(SocketType.ROUTER);
                ZMQ.Socket socket_dealer = context.createSocket(SocketType.DEALER);)

        {
            // request router port to bind and xsub to connect
            DistrictData dData = requestDistrictData(socket_req);

            if (dData == null) {
                System.out.println("[District:" + local + "] quiting...");
                return;
            }

            int Pub_port = dData.getPubPort();
            int Router_port = dData.getRouterPort();

            System.out.println("[District:" + local + "] Got data = " + dData.toString());

            socket_router.bind("tcp://*:" + Router_port);
            System.out.println("[District:" + local + "] router.bind = " + Router_port);

            String inproc_address = "inproc://threadWorkers";
            socket_dealer.bind(inproc_address);
            System.out.println("[District:" + local + "] inproc bind = " + inproc_address);

            // start N workers (config file)
            for (int i = 0; i < NUM_WORKERS; i++)
                new Worker(context, i, GLOBAL_MAP_LOCK, GLOBAL_MATRIX, GLOBAL_USER_POSITION, local, top5Positions,
                        Pub_port, LIMIT_USERS).start();

            // start proxy
            ZMQ.proxy(socket_router, socket_dealer, null);
        }
    }
}

class Worker extends Thread {

    private ZContext context;
    private int n;
    private int XSUB_LAYER2;
    private ReentrantLock lockMap;
    private UsersInPosition[][] globalMap;
    private ConcurrentHashMap<User, HashSet<String>> userPositions;
    private HttpClient httpclient;
    private TreeSet<Record> top5Positions;
    private static String DISTRICT;
    private static int LIMIT_USERS;

    private static ConfigReader config = new ConfigReader();
    private static String DIRETORIO_URI = config.getDiretorioURI();

    Worker(ZContext context, int n, ReentrantLock lockMap, UsersInPosition[][] globalmap,
            ConcurrentHashMap<User, HashSet<String>> userPositions, String district, TreeSet<Record> tr, int xsub,
            int limit) {
        this.context = context;
        this.n = n;
        this.lockMap = lockMap;
        this.globalMap = globalmap;
        this.userPositions = userPositions;
        this.httpclient = HttpClient.newHttpClient();
        this.top5Positions = tr;
        DISTRICT = district;
        this.XSUB_LAYER2 = xsub;
        LIMIT_USERS = limit;
    }

    public void run() {
        try (ZMQ.Socket socket_inproc = context.createSocket(SocketType.REP);
                ZMQ.Socket _XSUB_SOCKET = context.createSocket(SocketType.PUB)) {

            // connect to inproc dealer
            socket_inproc.connect("inproc://threadWorkers");

            // connect to notification brokers
            _XSUB_SOCKET.connect("tcp://*:" + this.XSUB_LAYER2);
            System.out.println("\t(worker " + n + ") xsub layer1 connected to port = " + this.XSUB_LAYER2);

            while (true) {

                byte[] msg = socket_inproc.recv();
                String request = new String(msg);

                if (request == null || request.length() == 0) {
                    System.out.println("\t(worker " + n + ") got ERROR|INVALID request = " + request);
                    socket_inproc.send("ERROR_INVALID_REQUEST\n");
                    continue;
                }

                System.out.println("\t(worker " + n + ") got request = " + request.replace("\n", ""));

                String partsRequest[] = request.replace("\n", "").split("_");

                if (partsRequest.length < 2) {
                    System.out.println("\t(worker " + n + ") got ERROR|INVALID request = " + request);
                    socket_inproc.send("ERROR_INVALID_REQUEST\n");
                    continue;
                }

                String username = partsRequest[0];

                switch (partsRequest[1]) {

                    // pedido 1: rcv: <username>_registo_<posx>_<posy> | snd: OK
                    case "registo":

                        this.lockMap.lock();
                        try {
                            // updating variables that track users

                            int posx = Integer.parseInt(partsRequest[2]);
                            int posy = Integer.parseInt(partsRequest[3]);

                            Position newPos = new Position(posx, posy);

                            User newUser = new User(username, newPos);

                            UsersInPosition uip = this.globalMap[posx][posy];

                            HashSet<String> contacts = uip.getUsersInPosition();

                            this.userPositions.put(newUser, contacts);
                            uip.addUser(username);

                            this.updateTop5Positions(newPos);

                            this.checkSendNotification(newPos, "too-many", _XSUB_SOCKET);

                            // updating rest api with post

                            ObjectMapper objectMapper = new ObjectMapper();
                            ObjectNode userDistrictObj = objectMapper.createObjectNode();

                            userDistrictObj.put("district", DISTRICT);
                            userDistrictObj.put("username", username);

                            String json = "";
                            try {

                                json = objectMapper.writerWithDefaultPrettyPrinter()
                                        .writeValueAsString(userDistrictObj);

                                HttpResponse<String> response = this.generic_request("POST",
                                        DIRETORIO_URI + "/user/reg", json);

                                System.out.println("\t(worker " + n + ") updated register user > " + username
                                        + ", response = " + response.statusCode());

                                socket_inproc.send("OK\n");

                            } catch (Exception e) {
                                socket_inproc.send("ERROR_UPDATE_DIRETORIO\n");
                                System.out.println("\t(worker " + n + ") could not update diretorio with = " + json);
                            }

                        } catch (Exception e) {
                            e.printStackTrace();
                            socket_inproc.send("ERROR_UPDATE_WORKER\n");
                        } finally {
                            this.lockMap.unlock();
                        }
                        break;

                    // pedido 2: rcv: <username>_track_<posx>_<posy> | snd: OK
                    case "track":

                        this.lockMap.lock();
                        try {

                            if (this.userPositions.containsKey(new User(username, new Position()))) {

                                int posx = Integer.parseInt(partsRequest[2]);
                                int posy = Integer.parseInt(partsRequest[3]);

                                Position newPos = new Position(posx, posy);

                                User oldUser = this.userPositions.keySet().stream()
                                        .filter(u -> u.getUsername().equals(username)).collect(Collectors.toList())
                                        .get(0);

                                // save user last position
                                Position oldPos = oldUser.getPos();

                                this.globalMap[oldUser.getPos().getPosX()][oldUser.getPos().getPosY()]
                                        .removeUser(username);
                                oldUser.setPos(new Position(posx, posy));

                                HashSet<String> newContacts = this.globalMap[posx][posy].getUsersInPosition();
                                this.userPositions.get(oldUser).addAll(newContacts);

                                for (String c : newContacts) {
                                    this.userPositions.get(new User(c, new Position())).add(username);
                                }

                                this.globalMap[posx][posy].addUser(username);

                                this.checkSendNotification(oldPos, "no-users", _XSUB_SOCKET);
                                this.checkSendNotification(newPos, "too-many", _XSUB_SOCKET);
                                this.checkSendNotification(oldPos, "less-users", _XSUB_SOCKET);

                                this.updateTop5Positions(newPos);

                                socket_inproc.send("OK\n");

                            } else {
                                socket_inproc.send("ERROR_USER_NOT_REGISTRED\n");

                            }

                        } finally {
                            this.lockMap.unlock();
                        }

                        break;

                    // pedido 3: rcv: <username>_infected | snd: OK
                    case "infected":

                        this.lockMap.lock();
                        try {

                            if (this.userPositions.containsKey(new User(username, new Position()))) {

                                // ---------------------------------------------------------------------------------------------------
                                // Communicate infection with diretorio api

                                ObjectMapper objectMapper = new ObjectMapper();
                                ObjectNode genericContactInfected = objectMapper.createObjectNode();

                                genericContactInfected.put("district", DISTRICT);
                                genericContactInfected.put("username", username);

                                String json = "";
                                try {
                                    json = objectMapper.writerWithDefaultPrettyPrinter()
                                            .writeValueAsString(genericContactInfected);
                                    HttpResponse<String> response = this.generic_request("PUT",
                                            DIRETORIO_URI + "/user/infection", json);
                                    System.out.println("\t(worker " + n + ") registred infection > " + username
                                            + ", response = " + response.statusCode());
                                } catch (Exception e) {
                                }

                                // send notification if an infection occured
                                _XSUB_SOCKET.send(DISTRICT + "_" + "infected");

                                // ---------------------------------------------------------------------------------------------------
                                // Communicate contacts and notify them

                                HashSet<String> contacts = this.userPositions.get(new User(username, new Position()));

                                // only update contacts and notify them if there are any
                                if (!contacts.isEmpty()) {

                                    genericContactInfected.remove("username");
                                    ArrayNode contactArray = genericContactInfected.putArray("users");

                                    contacts.stream().forEach(c -> contactArray.add(c));

                                    try {
                                        json = objectMapper.writerWithDefaultPrettyPrinter()
                                                .writeValueAsString(genericContactInfected);
                                        HttpResponse<String> response = this.generic_request("PUT",
                                                DIRETORIO_URI + "/user/contacts", json);
                                        System.out.println("\t(worker " + n + ") registred all contacts > " + username
                                                + ", response = " + response.statusCode());

                                    } catch (Exception e) {
                                    }

                                    // send notification of contact to all contacts
                                    contacts.stream().forEach(c -> _XSUB_SOCKET.send(c + "_" + "got-contact"));

                                    // remove user from the application
                                    this.userPositions.remove(new User(username, new Position()));
                                }

                                socket_inproc.send("OK_LOGOUT\n");

                            } else
                                socket_inproc.send("ERROR_USER_NOT_REGISTRED\n");

                        } finally {

                            this.lockMap.unlock();
                        }

                        break;

                    // pedido 4: rcv: <username>_n-users-in-pos_<x>_<y> | snd: OK_<number>
                    case "n-users-in-pos":

                        this.lockMap.lock();
                        try {

                            if (this.userPositions.containsKey(new User(username, new Position()))) {

                                int posx = Integer.parseInt(partsRequest[2]);
                                int posy = Integer.parseInt(partsRequest[3]);

                                int nr_users = this.globalMap[posx][posy].getNumberOfUsers();

                                String response = "OK_" + nr_users + "\n";

                                socket_inproc.send(response);
                            } else {
                                socket_inproc.send("ERROR_USER_NOT_REGISTRED\n");
                            }
                        } finally {

                            this.lockMap.unlock();
                        }

                        break;

                    default:

                        socket_inproc.send("ERROR_INVALID_CMD_TYPE\n");

                        break;
                }

            }
        }
    }

    public HttpResponse<String> generic_request(String typeRequest, String uri, String json) throws Exception {

        HttpResponse<String> response = null;

        switch (typeRequest) {

            case "POST":

                HttpRequest post = HttpRequest.newBuilder().uri(URI.create(uri))
                        .POST(HttpRequest.BodyPublishers.ofString(json)).header("Content-Type", "application/json")
                        .build();

                response = this.httpclient.send(post, HttpResponse.BodyHandlers.ofString());

                break;

            case "PUT":

                HttpRequest put = HttpRequest.newBuilder().uri(URI.create(uri))
                        .PUT(HttpRequest.BodyPublishers.ofString(json)).header("Content-Type", "application/json")
                        .build();

                response = this.httpclient.send(put, HttpResponse.BodyHandlers.ofString());
                break;
        }

        return response;
    }

    public void updateTop5Positions(Position newPos) {

        Record beforeRec = new Record(newPos, 0);

        int currentAmmountUsers = this.globalMap[newPos.getPosX()][newPos.getPosY()].getNumberOfUsers();

        // if last element of top 5 is greater than current amount of users in matrix
        if (!this.top5Positions.isEmpty() && this.top5Positions.last().getRecord() > currentAmmountUsers)
            return;

        boolean diretorioNeedsUpdate = false;

        int lastRecord = -1;

        // check if record should be updated
        if (this.top5Positions.contains(beforeRec)) {

            beforeRec = this.top5Positions.stream().filter(r -> r.getPos().equals(newPos)).collect(Collectors.toList())
                    .get(0);

            lastRecord = beforeRec.getRecord();

            if (beforeRec.getRecord() < currentAmmountUsers) {

                // update district top5

                this.top5Positions.remove(beforeRec);
                beforeRec.setRecord(currentAmmountUsers);
                this.top5Positions.add(beforeRec);

                // update diretorio

                diretorioNeedsUpdate = true;

            }

        } else {

            diretorioNeedsUpdate = true;

            this.top5Positions.add(beforeRec);

            if (this.top5Positions.size() > 5)
                this.top5Positions.pollLast();
        }

        if (diretorioNeedsUpdate) {

            ObjectMapper objectMapper = new ObjectMapper();
            ObjectNode updateTop5Obj = objectMapper.createObjectNode();

            updateTop5Obj.put("district", DISTRICT);
            updateTop5Obj.put("record", currentAmmountUsers);
            updateTop5Obj.put("positionX", newPos.getPosX());
            updateTop5Obj.put("positionY", newPos.getPosY());

            String json = "";
            try {
                json = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(updateTop5Obj);
                HttpResponse<String> response = this.generic_request("PUT", DIRETORIO_URI + "/top5", json);
                System.out.println("\t(worker " + n + ") updated " + DISTRICT + " TOP5 from " + lastRecord + " "
                        + DISTRICT + " to " + currentAmmountUsers + ", response = " + response.statusCode());
            } catch (Exception e) {
            }
        }
    }

    public void checkSendNotification(Position p, String type, ZMQ.Socket _XSUB_SOCKET) {

        int posx = p.getPosX(), posy = p.getPosY();

        UsersInPosition toNotifyNew = this.globalMap[posx][posy];

        switch (type) {

            case "no-users":

                if (toNotifyNew.getNumberOfUsers() == 0)
                    _XSUB_SOCKET.send(DISTRICT + "_" + "no-users_" + posx + "_" + posy);

                break;

            case "too-many":

                // send notification if position has more users than limit
                if (toNotifyNew.getSentLimit() == false && toNotifyNew.getNumberOfUsers() > LIMIT_USERS) {
                    _XSUB_SOCKET.send(DISTRICT + "_too-many_" + posx + "_" + posy);
                    toNotifyNew.setSentLimit(true);
                }

                break;

            case "less-users":

                // send notification if position has less users than limit
                if (toNotifyNew.getSentLimit() == true && toNotifyNew.getNumberOfUsers() < LIMIT_USERS) {

                    _XSUB_SOCKET.send(DISTRICT + "_less-users_" + posx + "_" + posy);
                    toNotifyNew.setSentLimit(false);
                }

                break;
        }

    }
}