package zeromq.pub_sub;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Subscriber {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.SUB))
        {
            socket.connect("tcp://localhost:" + args[0]);
            if (args.length == 1)
                socket.subscribe("".getBytes());
            else for (int i = 1; i < args.length; i++)
                socket.subscribe(args[i].getBytes());
            while (true) {
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}

