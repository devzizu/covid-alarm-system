package app.broker;

import org.zeromq.*;

import app.*;
import app.central.BrokerProtocol;
import org.apache.commons.lang3.SerializationUtils;

public class DynamicBrokerLayer2 {

    static ConfigReader config = new ConfigReader();

    public static BrokerProtocol requestDynamicBrokerLayer2(ZMQ.Socket socket_req) {

        // send : layer2
        // rcv : object - BrokerProtocol

        // request to central server port
        String port = config.getPort("ports", "CENTRAL_SERVER_REP");
        socket_req.connect("tcp://*:" + port);

        // create request message
        socket_req.send(new String("layer2"));
        byte[] msg_recv = socket_req.recv();
        BrokerProtocol message = SerializationUtils.deserialize(msg_recv);
        return message;

    }

    public static void main(String[] args) {

        try (ZContext context = new ZContext();
                // to request a dynamic layer 1 broker
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB)) {

            BrokerProtocol generated = requestDynamicBrokerLayer2(socket_req);

            // bind xpub
            int xpub = generated.getXPUB_PORT();
            XPUBSocket.bind("tcp://*:" + xpub);

            // connect to existing layer1 xpubs
            for (Integer xsub : generated.getXSUB_PORTS()) {
                XSUBSocket.connect("tcp://*:" + xsub);
            }

            int pubCS = Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_PUB"));
            XSUBSocket.connect("tcp://*:" + pubCS);

            System.out.println("[Broker - layer2] XPUB port = " + xpub);
            System.out.println("[Broker - layer2] XSUB port = " + generated.toString());

            // --------------------------------------------------------------------------

            new Proxy(context, XPUBSocket, XSUBSocket).poll();
        }
    }
}