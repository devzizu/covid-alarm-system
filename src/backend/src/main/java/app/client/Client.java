package app.client;

import java.io.*;
import java.net.Socket;

import org.zeromq.*;

import app.*;

public class Client {

    static ConfigReader config = new ConfigReader();

    static int PORT_FRONTEND = Integer.parseInt(config.getPort("ports", "FRONTEND"));

    

    public static void recv_notifications(BufferedReader reader) throws IOException {
        String msg = reader.readLine();
        String[] parts = msg.split("_");
        System.out.println(msg);
        System.out.println(parts[0]);
        if(parts[0].equals("pub")){
            try (ZContext context = new ZContext())
             {
                 int pub_port = Integer.parseInt(parts[1]);

                 //Thread que recebe notificações
                 new Notifications(context, pub_port);
             }
        } else System.out.println(msg);
    }

    public static void main(String[] args) {

        try {
            Socket socket = new Socket("localhost", PORT_FRONTEND);
            BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in));
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter writer = new PrintWriter(socket.getOutputStream());

            recv_notifications(reader);
            String msg = "";

            while((msg = stdIn.readLine()) != null){ 
                writer.println(msg);
                writer.flush();  
                
                //wait until response
                String response = reader.readLine(); 
                
                System.out.println("[Received]: " + response);
            }

            socket.close();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Notifications extends Thread {
    ZContext context;
    int pub_port;

    Notifications(ZContext context, int pub_port) {
        this.context = context;
        this.pub_port = pub_port;
    }

    public void run() {
        try (ZMQ.Socket socket = context.createSocket(SocketType.SUB)) {
            socket.connect("tcp://localhost:" + this.pub_port);
            
            while (true) {
                byte[] msg = socket.recv();
                String message = new String(msg);
                System.out.println("[Received] " + message);
            }
        }
    }
}

