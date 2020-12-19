
package app.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.ConfigReader;
import app.client.gui.GUI;

public class Client {

    static ConfigReader config = new ConfigReader();

    static int PORT_FRONTEND = Integer.parseInt(config.getPort("ports", "FRONTEND"));

    public static boolean recv_notifications(BufferedReader frontendSocketReader) throws IOException {

        // 1st request, block until it gets a notification socket
        System.out.println("[Client:app] Requesting notification socket...");

        String msg = frontendSocketReader.readLine();
        String[] parts = msg.split("_");
        System.out.println(msg);
        System.out.println(parts[0]);

        if (parts[0].equals("pub")) {

            try (ZContext context = new ZContext()) {

                int pub_port = Integer.parseInt(parts[1]);

                System.out.println("[Client:app] OK, notification socket: xpub = " + pub_port);

                // Thread que recebe notificações
                new Notifications(context, pub_port);
            }

            return true;

        } else // error generating xpub port
            System.out.println("[Client:app] (error) could not get a notification socket...");

        return false;
    }

    public static void main(String[] args) {

        try {

            System.out.println("[Client:app] started...");

            // Socket to talk with frontend
            Socket socket = new Socket("localhost", PORT_FRONTEND);
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter writer = new PrintWriter(socket.getOutputStream());

            if (!socket.isConnected()) {

                System.out.println("[Client:app] (error) connecting to frontend...");
                socket.close();
                return;
            }

            System.out.println("[Client:app] connected with frontend...");

            // Request and start notification sub zmq socket
            boolean canReceiveNotifications = recv_notifications(reader);

            if (!canReceiveNotifications) {
                socket.close();
                return;
            }

            // read client requests

            // Read from stdin
            BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in));

            String option = "";

            GUI.clear_terminal();
            GUI.main_menu();

            boolean stop_application = false, logged = false;

            while (!stop_application) {

                GUI.command_prompt("cmd", GUI.ANSI_GREEN);
                option = stdIn.readLine();

                int option_selected = -1;
                try {
                    option_selected = Integer.parseInt(option);
                } catch (NumberFormatException e) {
                    GUI.error("operation not permitted...");
                    continue;
                }

                String username, password, residencia, request, response;

                switch (option_selected) {

                    // login
                    case 0:

                        GUI.warning_no_nl("username: ");
                        username = stdIn.readLine();
                        GUI.warning_no_nl("password: ");
                        password = stdIn.readLine();

                        request = "login " + username + " " + password;

                        writer.println(request);
                        writer.flush();

                        break;

                    // register
                    case 1:

                        GUI.warning_no_nl("username: ");
                        username = stdIn.readLine();
                        GUI.warning_no_nl("password: ");
                        password = stdIn.readLine();
                        GUI.warning_no_nl("residencia: ");
                        residencia = stdIn.readLine();

                        request = "create " + username + " " + password + " " + residencia;

                        writer.println(request);
                        writer.flush();

                        break;

                    // clear
                    case 2:

                        GUI.command_prompt("operation", GUI.ANSI_GREEN);

                        request = stdIn.readLine();

                        writer.println(request);
                        writer.flush();

                        break;

                    // clear
                    case 3:
                        GUI.clear_terminal();
                        GUI.main_menu();
                        continue;

                    default:
                        GUI.error("operation not permitted...");
                        continue;
                }

                // wait until response from frontend
                response = reader.readLine();

                System.out.println(response);
            }

            // socket.close();

        } catch (Exception e) {

            e.printStackTrace();
        }

        System.out.println("[Client:frontend] app stoped...");
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

        try (ZMQ.Socket xpubSocket = context.createSocket(SocketType.SUB)) {

            xpubSocket.connect("tcp://localhost:" + this.pub_port);

            // needs to subscribe?
            // xpubSocket.subscribe(...);

            while (true) {
                byte[] msg = xpubSocket.recv();
                String message = new String(msg);
                System.out.println("[Client:app] Notification! data = " + message);
            }
        }

    }
}
