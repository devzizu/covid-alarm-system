
package app.central;

import org.apache.commons.lang3.SerializationUtils;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.*;
import app.broker.*;

import java.util.*;

public class CentralServer {

    // Map com key = Local referente ao district server
    // Value = Lista de portas a ser usadas para os diversos district server
    static Map<String, RoundRobinDistrict> localServers = new HashMap<>();

    // layer 1 broker list
    static List<Broker> layer1List = new ArrayList<>();
    static int current_layer1 = 0;

    // layer 2 broker list
    static List<Integer> layer2List = new ArrayList<>();
    static int current_layer2 = 0;

    // to read config file
    static ConfigReader config = new ConfigReader();
    static int NUM_DISTRICTS = Integer.parseInt(config.getDistricts());
    static int numBrokerLayer1 = Integer.parseInt(config.getPort("ports", "LAYER1_BROKER"));
    static int numBrokerLayer2 = Integer.parseInt(config.getPort("ports", "LAYER2_BROKER"));

    // broker object for layer 2 connection requests
    static BrokerProtocol brokerObj = new BrokerProtocol();

    public static String parseMsg(String data) {

        String finalResponse = "";
        String[] parts = data.split("_");
        String local;
        String responseHeader = "centralserver";

        if (parts[0].equals("init")) {

            // from=frontend;
            // rcv=init_pub
            // snd=centralserver_<ok|error>_<layer2-xpub>

            // round robin in layer 2 xpubs
            current_layer2 = (current_layer2 + 1) % layer2List.size();

            if (!layer2List.isEmpty()) {

                int PUB_PORT = layer2List.get(current_layer2);
                finalResponse += responseHeader + "_ok_" + PUB_PORT;

            } else {

                // theres no layer 2 sockets available for notifications
                finalResponse += responseHeader + "_error";
            }

        } else if (parts[0].equals("cliente")) {

            // from=frontend_client_process;
            // rcv=cliente_<user>_<local>
            // snd=centralserver_<ok|error>_<district-router>

            local = parts[2];

            // if there is district servers available, round robin in them
            if (localServers.containsKey(local)) {

                RoundRobinDistrict rrd = localServers.get(local);
                DistrictData choosenDistrict = rrd.getNextDistrictServer();

                finalResponse += responseHeader + "_ok_" + choosenDistrict.getRouterPort();

            } else
                finalResponse += responseHeader + "_error";

        } else if (parts[0].equals("district")) {

            // from=district;
            // rcv=district_<local>
            // snd=centralserver_<ok|error>_<district-router>_<layer1-xsub-connect>

            local = parts[1];
            String port_local = config.getPort("local", local);

            // not defined (tmp)
            int generatedRouterPort = -1;

            // round robin layer 1 xsubs
            current_layer1 = (current_layer1 + 1) % layer1List.size();
            int PUB_PORT = layer1List.get(current_layer1).getXSUB_PORT();

            // if a district server for that local is not available yet
            if (!localServers.containsKey(local)) {

                // add new district server
                generatedRouterPort = Integer.parseInt(port_local)
                        + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP"));
                localServers.put(local, new RoundRobinDistrict(
                        new ArrayList<>(Arrays.asList(new DistrictData(generatedRouterPort, PUB_PORT)))));

            } else {

                // for multiple district servers (obsolete)

                generatedRouterPort = Integer.parseInt(port_local) + (NUM_DISTRICTS * localServers.get(local).sizeL())
                        + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP"));
                localServers.get(local).appendDistrictServer(new DistrictData(generatedRouterPort, PUB_PORT));
            }

            finalResponse += responseHeader + "_ok_" + generatedRouterPort + "_" + PUB_PORT;

            System.out.println("[CentralServer:REP] (districts-update) list = " + localServers.toString());

        } else if (parts[0].equals("layer1")) {

            // from=layer1;
            // rcv=layer1
            // snd=centralserver_<ok|error?>_<xpub-bind>_<xsub-bind>

            int XPUB = numBrokerLayer1++;
            int XSUB = numBrokerLayer1++;

            layer1List.add(new Broker(XPUB, XSUB));

            finalResponse += responseHeader + "_ok_" + XPUB + "_" + XSUB;

        } else if (parts[0].equals("layer2")) {

            // from=layer2;
            // rcv=layer2
            // snd=centralserver_objectLayer2

            // get next xpub port to bind
            int XPUB = numBrokerLayer2++;
            layer2List.add(XPUB);

            // Envia objeto por pub para todos
            brokerObj.setPUB_PORT(XPUB);
            // send current layer1 xpubs, layer 2 connects to all
            brokerObj.setSUB_PORTs(layer1List);

            finalResponse += "objectLayer2";
        }

        return finalResponse;
    }

    public static void main(String[] args) {

        System.out.println("[CentralServer] started...");

        try (ZContext context = new ZContext();
                ZMQ.Socket socketRep = context.createSocket(SocketType.REP);
                ZMQ.Socket socketXPub = context.createSocket(SocketType.PUB)) {

            // read reply port from config file
            String REP_port = config.getPort("ports", "CENTRAL_SERVER_REP");
            // bind central server reply port
            socketRep.bind("tcp://*:" + REP_port);
            System.out.println("[CentralServer:REP] (bind) REP.port = " + REP_port);

            // forever read requests from clients, brokers or district servers
            while (true) {

                // receive msg
                String data = new String(socketRep.recv());

                System.out.println("[CentralServer:REP] (request) req_data = " + data);

                // create response
                String sndMsg = parseMsg(data.replace("\"", ""));

                if (sndMsg.equals("objectLayer2")) {

                    socketRep.send(SerializationUtils.serialize(brokerObj));
                    brokerObj = new BrokerProtocol();

                } else
                    socketRep.send(sndMsg);

                System.out.println("[CentralServer:REP] (response) res_data = " + sndMsg);
            }
        }
    }
}