package zeromq.chat_service_v2;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Server {

    public static void main(String[] args) {
    
        try (
            ZContext context = new ZContext();
            ZMQ.Socket socket_pull = context.createSocket(SocketType.PULL);
            ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
            ZMQ.Socket socket_req = context.createSocket(SocketType.REQ))
        {

            socket_req.connect("tcp://*:"+Config.CENTRAL_REQ);
            socket_req.send("server");
            byte[] byte_res = socket_req.recv();
            String response = new String(byte_res);

            System.out.println(response);

            if (response.startsWith("OK")) {

                String resParts[] = response.split("_");
                int PULL_PORT = Integer.parseInt(resParts[1]);
                int PUB_PORT = Integer.parseInt(resParts[2]);

                socket_pull.bind("tcp://*:"+PULL_PORT);
                socket_pub.bind("tcp://*:"+PUB_PORT);
    
                while(true) {
                    byte[] msg = socket_pull.recv();
                    System.out.println("broadcasting: " + new String(msg));
                    socket_pub.send(msg);
                }

            } else
                System.out.println("Could not bind server port with central server..."); 
        }
    }
}