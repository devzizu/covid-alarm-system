
package app.district;

import java.net.URI;
import java.net.http.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.*;

public class DistrictServer {

    // --------------------------------------------------------------------------------------------------------------

    static String local;
    static ConfigReader config = new ConfigReader();
    static int NUM_WORKERS = Integer.parseInt(config.getWorkers());

    // --------------------------------------------------------------------------------------------------------------

    static int MAP_SIZE = config.getMapSize();

    // shared global map with shared lock
    static ReentrantLock GLOBAL_MAP_LOCK = new ReentrantLock();
    static UsersInPosition[][] GLOBAL_MATRIX = new UsersInPosition[MAP_SIZE][MAP_SIZE];
    static ConcurrentHashMap<User, HashSet<String>> GLOBAL_USER_POSITION = new ConcurrentHashMap<>();

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
                ZMQ.Socket socket_dealer = context.createSocket(SocketType.DEALER);
                ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB))

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

            socket_pub.connect("tcp://*:" + Pub_port);
            System.out.println("[District:" + local + "] xsub layer1 connect = " + Pub_port);

            // start N workers (config file)
            for (int i = 0; i < NUM_WORKERS; i++)
                new Worker(context, i, GLOBAL_MAP_LOCK, GLOBAL_MATRIX, GLOBAL_USER_POSITION, local).start();

            // start proxy
            ZMQ.proxy(socket_router, socket_dealer, null);
        }
    }
}

class Worker extends Thread {

    private ZContext context;
    private int n;
    private ReentrantLock lockMap;
    private UsersInPosition[][] globalMap;
    private ConcurrentHashMap<User, HashSet<String>> userPositions;
    private HttpClient httpclient;
    private static String DISTRICT;

    private static ConfigReader config = new ConfigReader();
    private static String DIRETORIO_URI = config.getDiretorioURI();

    Worker(ZContext context, int n, ReentrantLock lockMap, UsersInPosition[][] globalmap,
            ConcurrentHashMap<User, HashSet<String>> userPositions, String district) {
        this.context = context;
        this.n = n;
        this.lockMap = lockMap;
        this.globalMap = globalmap;
        this.userPositions = userPositions;
        this.httpclient = HttpClient.newHttpClient();
        DISTRICT = district;
    }

    public void run() {
        try (ZMQ.Socket socket_inproc = context.createSocket(SocketType.REP)) {

            socket_inproc.connect("inproc://threadWorkers");

            while (true) {

                byte[] msg = socket_inproc.recv();
                String request = new String(msg);

                if (request == null || request.length() == 0) {
                    System.out.println("\t(worker " + n + ") got ERROR|INVALID request = " + request);
                    continue;
                }

                System.out.println("\t(worker " + n + ") got request = " + request.replace("\n", ""));

                String partsRequest[] = request.replace("\n", "").split("_");

                if (partsRequest.length < 2) {
                    System.out.println("\t(worker " + n + ") got ERROR|INVALID request = " + request);
                    continue;
                }

                String username = partsRequest[0];

                if (!this.userPositions.containsKey(new User(username, new Position()))) {

                    socket_inproc.send("ERROR_NOT_EXISTS\n");

                } else {

                    switch (partsRequest[1]) {

                        // pedido 1: rcv: <username>_registo_<posx>_<posy> | snd: OK
                        case "registo":

                            this.lockMap.lock();
                            try {

                                // updating variables that track users

                                int posx = Integer.parseInt(partsRequest[2]);
                                int posy = Integer.parseInt(partsRequest[3]);

                                User newUser = new User(username, new Position(posx, posy));

                                UsersInPosition uip = this.globalMap[posx][posy];

                                HashSet<String> contacts = uip.getUsersInPosition();

                                this.userPositions.put(newUser, contacts);
                                uip.addUser(username);

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
                                    System.out
                                            .println("\t(worker " + n + ") could not update diretorio with = " + json);
                                }

                            } catch (Exception e) {
                                socket_inproc.send("ERROR_UPDATE_WORKER\n");
                            } finally {
                                this.lockMap.unlock();
                            }
                            break;

                        // pedido 2: rcv: <username>_track_<posx>_<posy> | snd: OK
                        case "track":

                            this.lockMap.lock();
                            try {

                                int posx = Integer.parseInt(partsRequest[2]);
                                int posy = Integer.parseInt(partsRequest[3]);

                                User oldUser = this.userPositions.keySet().stream()
                                        .filter(u -> u.getUsername().equals(username)).collect(Collectors.toList())
                                        .get(0);

                                this.globalMap[oldUser.getPos().getPosX()][oldUser.getPos().getPosY()]
                                        .removeUser(username);
                                oldUser.setPos(new Position(posx, posy));
                                this.globalMap[posx][posy].addUser(username);

                                socket_inproc.send("OK\n");

                            } finally {
                                this.lockMap.unlock();
                            }

                            break;

                        // pedido 3: rcv: <username>_infected | snd: OK
                        case "infected":

                            this.lockMap.lock();
                            try {

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

                                // ---------------------------------------------------------------------------------------------------
                                // Communicate contacts and notify them

                                HashSet<String> contacts = this.userPositions.get(new User(username, new Position()));

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

                                // (TODO)
                                // send notifications
                                // for (String userInContact : contacts) {
                                // }

                                // remove user from the application
                                this.userPositions.remove(new User(username, new Position()));

                                socket_inproc.send("OK_LOGOUT\n");

                            } finally {

                                this.lockMap.unlock();
                            }

                            break;

                        // pedido 4: rcv: <username>_n-users-in-pos_<x>_<y> | snd: <number>
                        case "n-users-in-pos":

                            this.lockMap.lock();
                            try {

                                int posx = Integer.parseInt(partsRequest[2]);
                                int posy = Integer.parseInt(partsRequest[3]);

                                int nr_users = this.globalMap[posx][posy].getNumberOfUsers();

                                String response = "OK_" + nr_users + "\n";

                                socket_inproc.send(response);

                            } finally {

                                this.lockMap.unlock();
                            }

                            break;

                        default:

                            socket_inproc.send("ERROR_INVALID\n");

                            break;
                    }
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
}