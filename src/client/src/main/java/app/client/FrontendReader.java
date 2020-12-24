package app.client;

import java.io.BufferedReader;
import java.net.Socket;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.ConfigReader;

public class FrontendReader extends Thread {

    ZContext context;
    static ConfigReader config = new ConfigReader();
    Socket socket;
    BufferedReader reader;

    private String username = null;

    public FrontendReader(ZContext context, Socket socket, BufferedReader reader) {
        this.context = context;
        this.socket = socket;
        this.reader = reader;
    }

    @Override
    public void run() {

        try (ZMQ.Socket PUSH_SOCKET = context.createSocket(SocketType.PUSH)) {

            PUSH_SOCKET.connect("inproc://push_reader");

            while (true) {

                String receiveMessage = reader.readLine();

                if (username == null) {

                    PUSH_SOCKET.send(receiveMessage);

                } else {

                    // is notification
                    if (receiveMessage.startsWith(username)) {

                        // print notification
                        System.out.println("[Private Notification] " + receiveMessage);

                    } else // is response of request
                        PUSH_SOCKET.send(receiveMessage);
                }
            }

        } catch (Exception e) {
            // e.printStackTrace();
        }
    }

    public void setUsername(String username) {
        this.username = username;
    }

}
