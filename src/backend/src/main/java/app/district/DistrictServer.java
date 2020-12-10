
package app.district;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.*;

public class DistrictServer {

        static String local;

        public static ConfigReader config = new ConfigReader();

        public static DistrictData requestDistrictData(ZMQ.Socket socket_req){    
                    
            //send : district_LOCAL
            //rcv  : ok(error)_districtpull_?pub
            
            //request to central server port
            String port = config.getPort("ports", "CENTRAL_SERVER_REP");
            socket_req.connect("tcp://*:" + port);
 
            //create request message
            String message = "district_"+local;
            socket_req.send(message);
            byte[] msg_recv = socket_req.recv();
            String msg = new String(msg_recv);
            
            //error handling not defined in CentralServer...(fixme)
            String[] campos = msg.split("_");
            if(campos[1].equals("ok")){
                int pull_port = Integer.parseInt(campos[2]);
                int pub_port = Integer.parseInt(campos[3]);
                return new DistrictData(pull_port, pub_port);
            } else return null;
        }
    
        public static void main(String[] args) {
            
            local = args[0];
        
            try (
                ZContext context = new ZContext();
                ZMQ.Socket socket_pull = context.createSocket(SocketType.PULL);
                ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ))
            {
                DistrictData dData = requestDistrictData(socket_req);

                int Pub_port = dData.getPubPort();
                int Pull_port = dData.getPullPort();

                socket_pull.bind("tcp://*:" + Pull_port);
                socket_pub.bind("tcp://*:" + Pub_port);

                System.out.println("[District: " + local + "] Porta Pub: " + Pub_port);
                System.out.println("[District: " + local + "] Porta Pull: " + Pull_port);

                while(true) {
                    byte[] msg = socket_pull.recv();                        
                    System.out.println("[District: " + local + "] Broadcasting: " + new String(msg));
                    socket_pub.send(msg);
                }
            }
        }
}