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

        System.out.println("[Layer 2 Broker] started...");

        try (ZContext context = new ZContext();
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB)) {

            BrokerProtocol generatedBroker = requestDynamicBrokerLayer2(socket_req);

            // broker was requested successfully

            if (generatedBroker == null) {
                System.out.println("[Layer 2 Broker] (error) can't negociate broker request.");
                return;
            }

            int xpub = generatedBroker.getXPUB_PORT();
            System.out.println("[Layer 2 Broker] (centralserver) Got (bind) XPUB.port = " + xpub);

            // bind xpub zmq.socket
            XPUBSocket.bind("tcp://*:" + xpub);

            // connect to all layer 1 xpubs
            for (Integer xsub : generatedBroker.getXSUB_PORTS()) {
                XSUBSocket.connect("tcp://*:" + xsub);
            }

            System.out.println("[Layer 2 Broker] (connected) XSUB.ports = " + generatedBroker.getXSUB_PORTS());

            // start proxy between frontend and backend socket
            ZMQ.proxy(XSUBSocket, XPUBSocket, null);
        }
    }
}