package app.broker;

import org.zeromq.*;

import app.*;

public class DynamicBrokerLayer1 {

    static ConfigReader config = new ConfigReader();

    public static Broker requestDynamicBrokerLayer1(ZMQ.Socket socket_req){
        
        //send : layer1
        //rcv  : ok(error)_xpub_xsub

        //request to central server port
        String port = config.getPort("ports", "CENTRAL_SERVER_REP");
        socket_req.connect("tcp://*:" + port);

        //create request message
        socket_req.send(new String("layer1"));
        byte[] msg_recv = socket_req.recv();
        
        //error handling not defined in CentralServer...(fixme)
        String[] campos = new String(msg_recv).split("_");
        if(campos[1].equals("ok")){
            int xpub_port = Integer.parseInt(campos[2]);
            int xsub_port = Integer.parseInt(campos[3]);
            return new Broker(xpub_port, xsub_port);
        } else return null;
    }

    public static void main(String[] args) {
        
        try (
                ZContext context = new ZContext();
                //to request a dynamic layer 1 broker
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket XSUBSocket = context.createSocket(SocketType.XSUB);
                ZMQ.Socket XPUBSocket = context.createSocket(SocketType.XPUB))
            {

                Broker generatedBroker = requestDynamicBrokerLayer1(socket_req);

                if (generatedBroker == null) {
                    System.out.println("[Broker error] Could not generate broker from CentralServer....");
                    return;
                }

                //broker was requested successfully

                int xpub = generatedBroker.getXPUB_PORT();
                int xsub = generatedBroker.getXSUB_PORT();

                System.out.println("[Broker - layer1] XPUB port = " + xpub);
                System.out.println("[Broker - layer1] XSUB port = " + xsub);

                //bind broker ports
                XPUBSocket.bind("tcp://*:" + xpub);
                XSUBSocket.bind("tcp://*:" + xsub);

                ZMQ.proxy(XSUBSocket, XPUBSocket, null);
            }
    }
}