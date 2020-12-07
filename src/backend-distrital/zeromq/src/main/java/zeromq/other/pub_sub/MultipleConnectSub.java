package zeromq.pub_sub;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class MultipleConnectSub {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.SUB))
        {
            for (int i = 2; i < args.length; i++) {
                socket.subscribe(args[i].getBytes());
            }
            socket.connect("tcp://localhost:" + args[0]);
            socket.connect("tcp://localhost:" + args[1]);
            while (true) {
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}

