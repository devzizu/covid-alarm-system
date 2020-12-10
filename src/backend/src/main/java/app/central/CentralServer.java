
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
        static Map<String, List<Integer>> district_servers = new HashMap<>();

        static int nDistricts = 18;

        public static ConfigReader config = new ConfigReader();

        public static String parseMsg(String data){
            //cliente_username_residencia
            //district_residencia
            String res = "";
            String[] parts = data.split("_");
            String local;
            String port_local = "";
            int port_final = 0;
            if(parts[0].equals("cliente")){
                local = parts[2];
                port_local = config.getPort("local", local);

                port_final = Integer.parseInt(port_local) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER"));

            }
            else if(parts[0].equals("district")){
                local = parts[1];

                port_local = config.getPort("local", local);

                if(!district_servers.containsKey(local)){
                    port_final = Integer.parseInt(port_local) + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER"));
                }else{
                    int newNumberPort = Integer.parseInt(port_local) * (1 + district_servers.get(local).size());
                    port_final = newNumberPort + Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER")); 
                }

                if(district_servers.containsKey(local)){
                    district_servers.get(local).add(port_final);
                }else{
                    List<Integer> ldist = new ArrayList<>();
                    ldist.add(port_final);
                    district_servers.put(local, ldist);
                }
            }
            //centralserver_ok_PORTAPUSH_PORTAXPUB
            //centralserver_error
            if(port_local == null){

                res = "centralserver_error";
            }
            else{

                res = "centralserver_ok_" + port_final + "_10050";
            }

            return res;
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