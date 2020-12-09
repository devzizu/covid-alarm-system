package app;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.Config.ConfigReader;

public class DistrictServer {

        static String local;

        public static ConfigReader config = new ConfigReader();

        public static String callCentral(ZMQ.Socket socket_req){
            String port = config.getPort("ports", "CENTRAL_SERVER");
            socket_req.connect("tcp://*:" + port);
            String message = "district_"+local;
            socket_req.send(message);
            byte[] msg_recv = socket_req.recv();
            String msg = new String(msg_recv);
            
            //"centralserver_ok(error)_XSUBPORT"
            String[] campos = msg.split("_");
            if(campos[1].equals("ok")){
                return campos[2];
            }
            else return null;
        }
    
        public static void main(String[] args) {
            
            local = args[0];
        
            try (
                ZContext context = new ZContext();
                ZMQ.Socket socket_pull = context.createSocket(SocketType.PULL);
                ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ))
            {
                String pubPort = callCentral(socket_req);

                String port = config.getPort("local", local);
                socket_pull.bind("tcp://*:" + port);

                socket_pub.bind("tcp://*:" + pubPort);

                while(true) {
                        byte[] msg = socket_pull.recv();
                        System.out.println("broadcasting: " + new String(msg));
                        socket_pub.send(msg);
                }
            }
        }
}