
package app.district;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.*;

public class DistrictServer {

        static String local;

        static ConfigReader config = new ConfigReader();

        static int NUM_WORKERS = Integer.parseInt(config.getWorkers());



        public static DistrictData requestDistrictData(ZMQ.Socket socket_req){    
                    
            //send : district_LOCAL
            //rcv  : ok(error)_districtpull_?pub
            
            //request to central server port
            String port = config.getPort("ports", "CENTRAL_SERVER_REP");
            socket_req.connect("tcp://*:" + port);
 
            //create request message
            String message = "district_"+local;
            socket_req.send(message);
            byte[] msg_recv = socket_req.recv();
            String msg = new String(msg_recv);
            
            //error handling not defined in CentralServer...(fixme)
            String[] campos = msg.split("_");
            if(campos[1].equals("ok")){
                int router_port = Integer.parseInt(campos[2]);
                int pub_port = Integer.parseInt(campos[3]);
                return new DistrictData(router_port, pub_port);
            } else return null;
        }
    
        public static void main(String[] args) {
            
            local = args[0];
        
            try (
                ZContext context = new ZContext();
                ZMQ.Socket socket_req = context.createSocket(SocketType.REQ);
                ZMQ.Socket socket_router = context.createSocket(SocketType.ROUTER);
                ZMQ.Socket socket_dealer = context.createSocket(SocketType.DEALER);
                ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB))
                

            {
                DistrictData dData = requestDistrictData(socket_req);

                int Pub_port = dData.getPubPort();
                int Router_port = dData.getRouterPort();

                socket_router.bind("tcp://*:" + Router_port);
                socket_dealer.bind("inproc://threadWorkers");
                socket_pub.connect("tcp://*:" + Pub_port);

                System.out.println("[District: " + local + "] Porta connect XSUB layer1: " + Pub_port);
                System.out.println("[District: " + local + "] Porta Pull: " + Router_port);

                for (int i = 0; i < NUM_WORKERS; i++)
                    new Worker(context, i).start();
                    
                ZMQ.proxy(socket_router, socket_dealer, null);
            }
        }
}

class Worker extends Thread {
    ZContext context;
    int n;

    Worker(ZContext context, int n) {
        this.context = context;
        this.n = n;
    }

    public void run() {
        try (ZMQ.Socket socket_inproc = context.createSocket(SocketType.REP)) {
            socket_inproc.connect("inproc://threadWorkers");
            while (true) {
                byte[] msg = socket_inproc.recv();
                String pedido = new String(msg);
                System.out.println("[Worker " + n + "] received " + pedido);
                
                //do work

                socket_inproc.send(pedido);
            }
        }
    }
}
