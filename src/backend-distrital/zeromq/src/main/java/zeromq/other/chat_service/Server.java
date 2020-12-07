package zeromq.chat_service_v2;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Server {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
             ZMQ.Socket socket_pull = context.createSocket(SocketType.PULL))
        {
            socket_pub.bind("tcp://*:12345");
            socket_pull.bind("tcp://*:12346");
            while(true) {
                byte[] msg = socket_pull.recv();
                socket_pub.send(msg);
            }
        }
    }
}

