package app.broker;

import org.zeromq.*;

import app.*;
import app.central.BrokerProtocol;
import org.apache.commons.lang3.SerializationUtils;

public class DynamicBrokerLayer2_tmp {

    static ConfigReader config = new ConfigReader();

    public static BrokerProtocol requestDynamicBrokerLayer2(ZMQ.Socket socket_req){
        
        //send : layer2
        //rcv  : object - BrokerProtocol

        //request to central server port
        String port = config.getPort("ports", "CENTRAL_SERVER_REP");
        socket_req.connect("tcp://*:" + port);

        //create request message
        socket_req.send(new String("layer2"));
        byte[] msg_recv = socket_req.recv();
        BrokerProtocol message = SerializationUtils.deserialize(msg_recv);
        return message;
        
    }

    public static void main(String[] args) {
        
        try (
                ZContext context = new ZContext();
                //to request a dynamic layer 1 broker
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB))
            {

                BrokerProtocol generated = requestDynamicBrokerLayer2(socket_req);
            
                //broker was requested successfully

                int xpub = generated.getXPUB_PORT();
                XPUBSocket.bind("tcp://*:" + xpub);

                System.out.println("[Broker - layer2] XPUB port = " + xpub);
                System.out.println("[Broker - layer2] XSUB port = " + generated.toString());
                  
                new Thread(() -> {

                    try (
                        ZMQ.Socket SUBSocket = context.createSocket(SocketType.SUB)
                    ) {

                        //FIXME
                        //XSUBSocket.bind("tcp://*:"+(xpub+200));

                        for(Integer xsub: generated.getXSUB_PORTS()){
                            XSUBSocket.connect("tcp://*:" + xsub);
                        }

                        int pub = Integer.parseInt(config.getPort("ports", "CENTRAL_SERVER_PUB"));

                        SUBSocket.connect("tcp://*:"+pub);
                        SUBSocket.subscribe("layer2_".getBytes());
                        
                        while(true) {
                            System.out.println("[Broker - layer2, notifier] waiting for new notifications...");
                            byte[] resBytes = SUBSocket.recv();
                            String resMsg = new String(resBytes);
                            String[] parts = resMsg.split("_");
                            System.out.println("[Broker] received notification: " + (resMsg));
                            int newXPUB = Integer.parseInt(parts[1]);
                            XSUBSocket.connect("tcp://*:" + newXPUB);
                        }
                    }

                }).start();

                ZMQ.proxy(XSUBSocket, XPUBSocket, null);
            }  
    }
}