package zeromq.push_pull;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Worker {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket source = context.createSocket(SocketType.PULL);
             ZMQ.Socket sink = context.createSocket(SocketType.PUSH))
        {
            source.connect("tcp://localhost:" + args[0]);
            sink.connect("tcp://localhost:" + args[1]);
            while (true) {
                byte[] msg = source.recv();
                int n = Integer.parseInt(new String(msg));
                System.out.println("Received " + n);
                String res = String.valueOf(2 * n);
                sink.send(res);
            }
        }
    }
}

