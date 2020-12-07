package zeromq.request_reply;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class EchoServer {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REP))
        {
            for (String port : args)
                socket.bind("tcp://*:" + port);
            while (true) {
                byte[] msg = socket.recv();
                String str = new String(msg);
                System.out.println("Received " + str);
                String res = str.toUpperCase();
                socket.send(res);
            }
        }
    }
}

