package zeromq.push_pull;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Sink {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket sink = context.createSocket(SocketType.PULL))
        {
            sink.bind("tcp://*:" + args[0]);
            byte[] msg;
            try (ZMQ.Socket srv = context.createSocket(SocketType.REP)) {
                srv.bind("tcp://*:" + args[1]);
                msg = srv.recv();
                srv.send(""); // reply as ack
            }
            int nMsgs = Integer.parseInt(new String(msg));
            System.out.println("Going to receive " + nMsgs + " messages");
            int sum = 0;
            for (int i = 0; i < nMsgs; i++) {
                msg = sink.recv();
                String str = new String(msg);
                System.out.println("Received " + str);
                sum += Integer.parseInt(str);
            }
            System.out.println(sum);
        }
    }
}
