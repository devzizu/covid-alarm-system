package zeromq.request_reply;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class EchoClient {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REQ))
        {
            for (String port : args)
                socket.connect("tcp://localhost:" + port);
            while (true) {
                String str = System.console().readLine();
                if (str == null) break;
                socket.send(str);
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}

