package zeromq.pub_sub_broker;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Publisher {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.connect("tcp://localhost:" + args[0]);
            while (true) {
                String str = System.console().readLine();
                if (str == null) break;
                socket.send(str);
            }
        }
    }
}

