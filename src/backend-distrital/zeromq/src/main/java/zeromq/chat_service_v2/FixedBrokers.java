package zeromq.chat_service_v2;
import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class FixedBrokers {

    public static class Broker implements Runnable {

        public int XPUB_PORT;
        public int XSUB_PORT;

        public Broker(int port) {
            this.XPUB_PORT = port;
            this.XSUB_PORT = port + Config.Servers.size();
        }

        public void run() {

            System.out.println("Broker " + (this.XPUB_PORT-Config.Servers.get(0)) + "started,");

            try (
                ZContext context = new ZContext();
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB);
                ZMQ.Socket SUBSocket = context.createSocket(SocketType.SUB))
                {
                XPUBSocket.bind("tcp://*:"+this.XPUB_PORT);
                XSUBSocket.bind("tcp://*:"+this.XSUB_PORT);

                new Thread(() -> {
                    
                    boolean connected = SUBSocket.connect("tcp://*:"+Config.CENTRAL_PUB);
                    SUBSocket.subscribe("".getBytes());
                    System.out.println("Connected="+connected);
                    while(true) {
                        byte[] res = SUBSocket.recv();
                        System.out.println("received");
                        int NEW_PUB = Integer.parseInt(new String(res));
                        System.out.println("Connected to a new Publisher: "+NEW_PUB);
                        XSUBSocket.connect("tcp://*:"+NEW_PUB);
                    }

                }).start();

                ZMQ.proxy(XSUBSocket, XPUBSocket, null);
            }
        }
    }
    
    public static void main(String[] args) {
        
        System.out.println("Starting fixed brokers...");

        for(int broker = 0; broker < Config.Servers.size(); broker++) {

            Broker br = new Broker(Config.Servers.get(broker));

            new Thread(br).start();
        }
    }
}
