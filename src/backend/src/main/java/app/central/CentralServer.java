
package app.central;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.ConfigReader;

import java.util.*;

public class CentralServer {

        //Map com key = Local referente ao district server
        //Value = Lista de portas a ser usadas para os diversos district server
        //A Porta é referente ao PULL
        config = new ConfigReader();
        static Map<String, RoundRobinDistrict> localServers = new HashMap<>();

        static int NOT_DEFINED_PUB = 10999;
        static int NUM_DISTRICTS   =  Integer.parseInt(config.getNumDistricts());
        

        public static String parseMsg(String data){
            
            //cliente_username_residencia
            //district_residencia
            
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

                    finalResponse += responseHeader + "_ok_" + choosenDistrict.getPullPort() + "_" + choosenDistrict.getPubPort();

                } else  finalResponse += responseHeader + "_error"; 

            } else if(parts[0].equals("district")) {

                local = parts[1];

                //district_Porto

                port_local = config.getPort("local", local);

                int generatedPullPort = -1; //not defined

                if(!localServers.containsKey(local)){

                    generatedPullPort = Integer.parseInt(port_local) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER"));
                    localServers.put(local, new RoundRobinDistrict(new ArrayList<>(Arrays.asList(new DistrictData(
                        generatedPullPort, NOT_DEFINED_PUB
                    )))));    
                
                } else {
                    
                    generatedPullPort = Integer.parseInt(port_local) + (NUM_DISTRICTS * district_servers.get(local).size()) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER")); 
                    localServers.get(local).appendDistrictServer(new DistrictData(
                        generatedPullPort, NOT_DEFINED_PUB
                    ));
                }

                finalResponse += "_ok_" + generatedPullPort + "_" + NOT_DEFINED_PUB;
            }

            return finalResponse;
        }
    
        public static void main(String[] args) {

            try (
                ZContext context = new ZContext();
                ZMQ.Socket socketRep = context.createSocket(SocketType.REP))
            {
                String port = config.getPort("ports", "CENTRAL_SERVER");
                socketRep.bind("tcp://*:" + port);

                System.out.println("Starting Central Server on port " + port + "...");

                while(true){
                    String data = new String(socketRep.recv());
                    System.out.println("Received: " + data);
                    String sndMsg = parseMsg(data.replace("\"", ""));
                    System.out.println("Sending: " + sndMsg);
                    socketRep.send(sndMsg);
                }
            }
        }
}