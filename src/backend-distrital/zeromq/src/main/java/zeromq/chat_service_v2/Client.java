package zeromq.chat_service_v2;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Scanner;

public class Client {

    static String room = "default";

    public static void main(String[] args) {
        try (
            ZContext context = new ZContext();
            ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
            ZMQ.Socket socket_push = context.createSocket(SocketType.PUSH);
            ZMQ.Socket socket_req = context.createSocket(SocketType.REQ))
        {

            //-------------------------------------------------------------------------------

            //connect to central server
            socket_req.connect("tcp://*:"+Config.CENTRAL_REQ);

            socket_req.send("client");
            String resStr = new String(socket_req.recv());

            int PULL_PORT = -1, BROKER_PORT = -1;
            if (resStr.startsWith("OK")) {
                String[] partsResponse = resStr.split("_");
                PULL_PORT = Integer.parseInt(partsResponse[1]);
                BROKER_PORT = Integer.parseInt(partsResponse[2]);
                System.out.println("OK: PULL="+PULL_PORT+",BROKER="+BROKER_PORT);
            } else if (resStr.startsWith("ERROR")) {
                System.out.println("ERROR: No server available to connect...");
                return;
            }

            //-------------------------------------------------------------------------------

            socket_pub.bind("inproc://comunicador");
            new ChangeRoom(context, BROKER_PORT).start();

            //-------------------------------------------------------------------------------

            //connect to dynamic server pull port
            socket_push.connect("tcp://*:"+PULL_PORT);

            //read client msgs
            Scanner scan = new Scanner(System.in);
            String line, room2;
            while ((line = scan.nextLine())!=null) {
                if(line.startsWith("room ")){
                    String[] parse = line.split(" ");
                    if(parse.length >= 2){
                        room2 = parse[1];
                        socket_pub.send("chr_"+room+"_to_"+room2);
                        room=room2;
                    }
                }
                else{
                    socket_push.send("msg_"+room+"_"+line);
                }

            }
        }
    }
}

class ChangeRoom extends Thread {

    ZContext context;
    int XPUB_PORT;

    ChangeRoom(ZContext context, int pub_port) {
        this.context = context;
        this.XPUB_PORT = pub_port;
    }

    public void run() {
        
        try (ZMQ.Socket socket_sub = context.createSocket(SocketType.SUB)) {
            socket_sub.connect("inproc://comunicador");
            socket_sub.connect("tcp://*:"+this.XPUB_PORT);
            socket_sub.subscribe("msg_default");
            socket_sub.subscribe("chr_");

            while (true) {

                String line = new String(socket_sub.recv());
                String[] parse = line.split("_");

                if(line.startsWith("chr_")){
                    socket_sub.unsubscribe("msg_"+parse[1]);
                    socket_sub.subscribe("msg_"+parse[3]);
                }
                else{
                    System.out.println("["+parse[1]+"] " + parse[2]);
                }
            }
        }
    }
}