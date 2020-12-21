
package app.client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Notifications extends Thread {
    ZContext context;
    int pub_port;
    ZMQ.Socket xpubSocket;

    Notifications(ZContext context, int pub_port) {
        this.context = context;
        this.pub_port = pub_port;
    }

    public void run() {

        try (ZMQ.Socket xpubS = context.createSocket(SocketType.SUB)) {

            this.xpubSocket = xpubS;

            this.xpubSocket.connect("tcp://localhost:" + this.pub_port);

            while (true) {
                byte[] msg = this.xpubSocket.recv();
                String message = new String(msg);
                System.out.println("[Client:app] Notification! data = " + message);
            }
        }

    }

    public void subscribe(String subtopic) {
        this.xpubSocket.subscribe(subtopic);
    }
}
