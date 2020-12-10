
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
    static int NOT_DEFINED_PUB = 10999;
    static int NUM_DISTRICTS   =  Integer.parseInt(config.getNumDistricts());

    static BrokerProtocol brokerObj = new BrokerProtocol();

    public static String parseMsg(String data){
        
        //send : cliente_username_residencia
        //rcv  : ok(error)_districtpull_?pub
        //send : district_residencia
        //rcv  : ok(error)_districtpull_?pub
        
        String finalResponse = "";
        String[] parts = data.split("_");
        String local;
        String responseHeader = "centralserver";
        
        //client requests ports to connect
        if(parts[0].equals("cliente")) {
            
            local = parts[2];

            //if there is district servers available, round robin in them
            if (localServers.containsKey(local)) {

                RoundRobinDistrict rrd = localServers.get(local);
                DistrictData choosenDistrict = rrd.getNextDistrictServer();

                current_layer2 = (current_layer2 + 1) % layer2List.size();
                int PUB_PORT = layer2List.get(current_layer2);

                finalResponse += responseHeader + "_ok_" + choosenDistrict.getPullPort() + "_" + PUB_PORT;

            } else  finalResponse += responseHeader + "_error"; 

        } else if(parts[0].equals("district")) {

            local = parts[1];

            //district_Porto

            String port_local = config.getPort("local", local);

            int generatedPullPort = -1; //not defined

            current_layer1 = (current_layer1 + 1) % layer1List.size();
            int PUB_PORT = layer1List.get(current_layer1).getXSUB_PORT();


            if(!localServers.containsKey(local)){

                generatedPullPort = Integer.parseInt(port_local) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP"));
                localServers.put(local, new RoundRobinDistrict(new ArrayList<>(Arrays.asList(new DistrictData(
                    generatedPullPort, PUB_PORT
                )))));    
            
            } else {
                
                generatedPullPort = Integer.parseInt(port_local) + (NUM_DISTRICTS * localServers.get(local).sizeL()) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_REP")); 
                localServers.get(local).appendDistrictServer(new DistrictData(
                    generatedPullPort, PUB_PORT
                ));
            }

            finalResponse += "_ok_" + generatedPullPort + "_" + NOT_DEFINED_PUB++;

            System.out.println("-> District Servers: \n" + localServers.toString());

        } else if(parts[0].equals("layer1")) {
                
            int numBroker = Integer.parseInt(config.getPort("ports", "LAYER1_BROKER"));
            int XPUB = numBroker;
            int XSUB = numBroker++;
            layer1List.add(new Broker(XPUB, XSUB));

            finalResponse += "_ok_"+ XPUB + "_" +XSUB;

        } else if(parts[0].equals("layer2")) {
                
            int numBroker = Integer.parseInt(config.getPort("ports", "LAYER2_BROKER"));
            int XPUB = numBroker;
            layer2List.add(XPUB);

            //Envia objeto por pub para todos 
            brokerObj.setPUB_PORT(XPUB);
            brokerObj.setSUB_PORTs(layer1List);

            finalResponse += "objectLayer2";

    }

        return finalResponse;
    }

    public void sendPub(int XPUB){

    }

    public static void main(String[] args) {

        try (
            ZContext context = new ZContext();
            ZMQ.Socket socketRep = context.createSocket(SocketType.REP);
            ZMQ.Socket socketPub = context.createSocket(SocketType.PUB))
        {
            String port = config.getPort("ports", "CENTRAL_SERVER_REP");
            socketRep.bind("tcp://*:" + port);

            System.out.println("Starting Central Server on port " + port + "...");

            while(true) {
                String data = new String(socketRep.recv());
                System.out.println("Received: " + data);
                String sndMsg = parseMsg(data.replace("\"", ""));
                if(sndMsg.equals("objectLayer2")){
                    socketRep.send(SerializationUtils.serialize(brokerObj));
                }else{
                    a = 2;
                    socketRep.send(sndMsg);
                }
                System.out.println("Sending: " + sndMsg);
            }
        }
    }
}