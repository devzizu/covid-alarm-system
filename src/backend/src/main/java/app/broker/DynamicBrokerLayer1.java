package app.broker;

import org.zeromq.*;

import app.*;

public class DynamicBrokerLayer1 {

    // to read config values
    static ConfigReader config = new ConfigReader();

    public static Broker requestDynamicBrokerLayer1(ZMQ.Socket socket_req) {

        // get central server port from config file
        String port = config.getPort("ports", "CENTRAL_SERVER_REP");
        // connect to central server
        socket_req.connect("tcp://*:" + port);

        // request msg of type: "layer1"
        String request = "layer1";

        System.out.println("[Layer 1 Broker] (centralserver) connecting and requesting <" + request + ">");

        socket_req.send(request);
        byte[] msg_recv = socket_req.recv();

        // error handling not defined in CentralServer...(fixme)
        String[] campos = new String(msg_recv).split("_");
        if (campos[1].equals("ok")) {
            int xpub_port = Integer.parseInt(campos[2]);
            int xsub_port = Integer.parseInt(campos[3]);
            return new Broker(xpub_port, xsub_port);
        } else
            return null;
    }

    public static void main(String[] args) {

        System.out.println("[Layer 1 Broker] started...");

        try (ZContext context = new ZContext();
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB)) {

            Broker generatedBroker = requestDynamicBrokerLayer1(socket_req);

            // broker was requested successfully

            if (generatedBroker == null) {
                System.out.println("[Layer 1 Broker] (error) can't negociate broker request.");
                return;
            }

            // broker was requested successfully
            int xpub = generatedBroker.getXPUB_PORT();
            int xsub = generatedBroker.getXSUB_PORT();

            System.out.println("[Layer 1 Broker] (centralserver) Got (bind) XPUB.port = " + xpub);
            System.out.println("[Layer 1 Broker] (centralserver) Got (bind) XSUB.port = " + xsub);

            // bind broker ports
            XPUBSocket.bind("tcp://*:" + xpub);
            XSUBSocket.bind("tcp://*:" + xsub);

            // start proxy
            ZMQ.proxy(XSUBSocket, XPUBSocket, null);
        }
    }
}