package zeromq.request_reply_broker;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class ClientWithId {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REQ))
        {
            socket.setIdentity(args[1].getBytes());
            socket.connect("tcp://localhost:"+args[0]);
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

