package zeromq.request_reply_broker;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class PollThreadedEchoServer {
    public static void main(String[] args) {
        int nWorkers = Integer.parseInt(args[1]);
        try (ZContext context = new ZContext();
             ZMQ.Socket clients = context.createSocket(SocketType.ROUTER);
             ZMQ.Socket workers = context.createSocket(SocketType.DEALER))
        {
            clients.bind("tcp://*:"+args[0]);
            workers.bind("inproc://workers");
            for (int i = 0; i < nWorkers; i++)
                new EchoWorker(context, i).start();
            new Proxy(context, clients, workers).poll();
        }
    }
}

class EchoWorker extends Thread {
    ZContext context;
    int n;

    EchoWorker(ZContext context, int n) {
        this.context = context;
        this.n = n;
    }

    public void run() {
        try (ZMQ.Socket socket = context.createSocket(SocketType.REP)) {
            socket.connect("inproc://workers");
            while (true) {
                byte[] msg = socket.recv();
                String str = new String(msg);
                System.out.println("Worker " + n + " received " + str);
                try { Thread.sleep(1000); } catch (Exception e) {} // simulate working
                String res = str.toUpperCase();
                socket.send(res);
            }
        }
    }
}

