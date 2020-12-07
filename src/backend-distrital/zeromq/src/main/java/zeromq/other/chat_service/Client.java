package zeromq.chat_service;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import zeromq.guiaoAggregationService.Config;

import java.util.Scanner;

public class Client {
    public static String room = "default";

    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket_pub = context.createSocket(SocketType.PUB);
             ZMQ.Socket socket_push = context.createSocket(SocketType.PUSH))
        {
            socket_push.connect("tcp://localhost:12346");
            socket_pub.bind("inproc://comunicador");

            new ChangeRoom(context).start();

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

    ChangeRoom(ZContext context) {
        this.context = context;
    }

    public void run() {
        try (ZMQ.Socket socket_sub = context.createSocket(SocketType.SUB)) {
            socket_sub.connect("inproc://comunicador");
            socket_sub.connect("tcp://localhost:12345");
            socket_sub.subscribe("msg_default");
            socket_sub.subscribe("chr_");

            while (true) {
                byte[] msg = socket_sub.recv();

                String line = new String(msg);

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