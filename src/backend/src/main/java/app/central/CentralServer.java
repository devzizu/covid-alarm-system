
package app.central;

import org.apache.commons.lang3.SerializationUtils;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.*;
import app.broker.*;

import java.util.*;

public class CentralServer {

    //Map com key = Local referente ao district server
    //Value = Lista de portas a ser usadas para os diversos district server
    static Map<String, RoundRobinDistrict> localServers = new HashMap<>();

    static List<Broker> layer1List = new ArrayList<>();
    static int current_layer1 = 0;

    static List<Integer> layer2List = new ArrayList<>();
    static int current_layer2 = 0;

    static ConfigReader config = new ConfigReader();

    static int NUM_DISTRICTS   =  Integer.parseInt(config.getDistricts());

    static BrokerProtocol brokerObj = new BrokerProtocol();

    static int numBrokerLayer1 = Integer.parseInt(config.getPort("ports", "LAYER1_BROKER"));
    static int numBrokerLayer2 = Integer.parseInt(config.getPort("ports", "LAYER2_BROKER"));

    public static String parseMsg(String data){
        
        //send : cliente_username_residencia
        //rcv  : ok(error)_districtpull_?pub
        //send : district_residencia
        //rcv  : ok(error)_districtpull_?pub
        
        String finalResponse = "";
        String[] parts = data.split("_");
        String local;
        String responseHeader = "centralserver";

        if(parts[0].equals("init")){
            //if (current_layer2 == -1) current_layer2 = 0;
            current_layer2 = (current_layer2 + 1) % layer2List.size();

            int PUB_PORT = layer2List.get(current_layer2);

            finalResponse += responseHeader + "_ok_" + PUB_PORT;

        //client requests ports to connect
        } else if(parts[0].equals("cliente")) {
            
            local = parts[2];

            //if there is district servers available, round robin in them
            if (localServers.containsKey(local)) {

                RoundRobinDistrict rrd = localServers.get(local);
                DistrictData choosenDistrict = rrd.getNextDistrictServer();

                finalResponse += responseHeader + "_ok_" + choosenDistrict.getRouterPort();

            } else  finalResponse += responseHeader + "_error"; 

        } else if(parts[0].equals("district")) {

            local = parts[1];

            //district_Porto

            String port_local = config.getPort("local", local);

            int generatedRouterPort = -1; //not defined

            //if (current_layer1 == -1) current_layer1 = 0;
            current_layer1 = (current_layer1 + 1) % layer1List.size();

            int PUB_PORT = layer1List.get(current_layer1).getXSUB_PORT();


            if(!localServers.containsKey(local)){

                generatedRouterPort = Integer.parseInt(port_local) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP"));
                localServers.put(local, new RoundRobinDistrict(new ArrayList<>(Arrays.asList(new DistrictData(
                    generatedRouterPort, PUB_PORT
                )))));    
            
            } else {
                
                generatedRouterPort = Integer.parseInt(port_local) + (NUM_DISTRICTS * localServers.get(local).sizeL()) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP")); 
                localServers.get(local).appendDistrictServer(new DistrictData(
                    generatedRouterPort, PUB_PORT
                ));
            }

            finalResponse += responseHeader + "_ok_" + generatedRouterPort + "_" + PUB_PORT;

            System.out.println("-> District Servers: \n" + localServers.toString());

        } else if(parts[0].equals("layer1")) {
                
            int XPUB = numBrokerLayer1++;
            int XSUB = numBrokerLayer1++;
            layer1List.add(new Broker(XPUB, XSUB));

            finalResponse += responseHeader + "_ok_"+ XPUB + "_" +XSUB;

        } else if(parts[0].equals("layer2")) {
                
            int XPUB = numBrokerLayer2++;
            layer2List.add(XPUB);

            //Envia objeto por pub para todos 
            brokerObj.setPUB_PORT(XPUB);
            brokerObj.setSUB_PORTs(layer1List);

            finalResponse += "objectLayer2";

    }

        return finalResponse;
    }

    public static void main(String[] args) {

        try (
            ZContext context = new ZContext();
            ZMQ.Socket socketRep = context.createSocket(SocketType.REP);
            ZMQ.Socket socketXPub = context.createSocket(SocketType.PUB))
        {
            String port = config.getPort("ports", "CENTRAL_SERVER_REP");
            socketRep.bind("tcp://*:" + port);
            String pubPort = config.getPort("ports", "CENTRAL_SERVER_PUB");
            socketXPub.bind("tcp://*:" + pubPort);

            System.out.println("Starting Central Server REP on port " + port + "...");
            System.out.println("Starting Central Server PUB on port " + pubPort + "...");

            while(true) {
                String data = new String(socketRep.recv());
                System.out.println("Received: " + data);
                String sndMsg = parseMsg(data.replace("\"", ""));
                if(sndMsg.equals("objectLayer2")){
                    socketRep.send(SerializationUtils.serialize(brokerObj));
                    brokerObj = new BrokerProtocol();
                }else socketRep.send(sndMsg);
                System.out.println("Sending through REQ-REP: " + sndMsg);
            }
        }
    }
}