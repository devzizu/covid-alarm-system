package zeromq.push_pull;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Source {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket source = context.createSocket(SocketType.PUSH))
        {
            source.bind("tcp://*:" + args[0]);
            //Thread.sleep(1000);  // wait for workers to connect
            int nMsgs = Integer.parseInt(args[2]);
            try (ZMQ.Socket cli = context.createSocket(SocketType.REQ)) {
                cli.connect("tcp://localhost:" + args[1]);
                cli.send(args[2]);
                cli.recv(); // receive reply
            }
            for (int i = 0; i < nMsgs; i++) {
                String str = String.valueOf(i);
                source.send(str);
                System.out.println("Sent " + str);
            }
        }
    }
}

