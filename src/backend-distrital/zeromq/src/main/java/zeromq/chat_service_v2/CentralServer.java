package zeromq.chat_service_v2;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.*;

public class CentralServer {

    private static class DynamicServer implements Comparable<DynamicServer> {

        public int nr_clients;
        public int pull_port;

        public DynamicServer(int port) {
            this.pull_port = port;
            this.nr_clients = 0;
        }

        public int compareTo(DynamicServer d) {
            if (this.nr_clients > d.nr_clients)
                return 1;
            if (this.nr_clients < d.nr_clients)
                return -1;
            return 0;
        }
    }

    static TreeSet<DynamicServer> dynamic_servers = new TreeSet<>();
    static int lastBroker = 0;
    static int lastDynamicBroker = 0;

    public static void main(String[] args) {
    
        try (
                ZContext context = new ZContext();
                ZMQ.Socket socketREP = context.createSocket(SocketType.REP);
                ZMQ.Socket socketPUB = context.createSocket(SocketType.PUB))
        {
            
            socketREP.bind("tcp://*:"+Config.CENTRAL_REQ);
            socketPUB.bind("tcp://*:"+Config.CENTRAL_PUB);

            while(true) {

                byte[] msg = socketREP.recv();
                String str = new String(msg);

                System.out.println("Received: " + str);

                if (str.startsWith("client")) {

                    DynamicServer lowerS = dynamic_servers.pollFirst();
                    
                    if (lowerS != null) {

                        StringBuilder sb = new StringBuilder();
                        sb.append("OK_");
                        sb.append(lowerS.pull_port);
                        sb.append("_");
                        sb.append(Config.Servers.get(lastBroker));
                        lastBroker=(lastBroker+1)%Config.Servers.size();

                        System.out.println("Sent: " + sb.toString());

                        socketREP.send(sb.toString());
                
                        lowerS.nr_clients++;
    
                        dynamic_servers.add(lowerS);

                    } else {
                        
                        //no servers available
                        socketREP.send("ERROR");
                    }

                } else if (str.startsWith("server")) {

                    //ler porta pull
                    int new_pull_port = Config.BASE_PORT_PULL_DYNAMIC + dynamic_servers.size();
                    int new_pub_port = Config.BASE_PORT_XPUB_DYNAMIC + dynamic_servers.size();

                    DynamicServer newServer = new DynamicServer(new_pull_port);
                    dynamic_servers.add(newServer);

                    StringBuilder sb = new StringBuilder();
                    sb.append("OK_");
                    sb.append(new_pull_port);
                    sb.append("_");
                    sb.append(new_pub_port);

                    //send info to dynamic servers
                    socketREP.send(sb.toString());

                    //send info to fixed brokers
                    socketPUB.send(new_pub_port + "");

                    System.out.println("Sent: " + sb.toString());
                }

            }

        }
    }
}