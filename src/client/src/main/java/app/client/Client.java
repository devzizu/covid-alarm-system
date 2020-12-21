
package app.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import org.zeromq.ZContext;

import app.ConfigReader;
import app.client.gui.GUI;

public class Client {

    static ConfigReader config = new ConfigReader();
    static int PORT_FRONTEND = Integer.parseInt(config.getPort("ports", "FRONTEND"));

    static Notifications notificationThread = null;
    static API API = null;

    public static boolean recv_notifications(BufferedReader frontendSocketReader) throws IOException {

        // 1st request, block until it gets a notification socket
        System.out.println("[Client:app] Requesting notification socket...");

        String msg = frontendSocketReader.readLine();
        String[] parts = msg.split("_");

        if (parts[0].equals("pub")) {

            try (ZContext context = new ZContext()) {

                int pub_port = Integer.parseInt(parts[1]);

                System.out.println("[Client:app] OK, notification socket: xpub = " + pub_port);

                // Thread que recebe notificações
                notificationThread = new Notifications(context, pub_port);
                notificationThread.start();
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

            API = new API(reader, writer, notificationThread);

            if (!canReceiveNotifications) {
                socket.close();
                return;
            }

            // read client requests

            // Read from stdin
            BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in));

            String option = "";

            GUI.clear_terminal();
            GUI.main_menu("startup", null);

            boolean stop_application = false;

            while (!stop_application) {

                GUI.command_prompt("cmd", GUI.ANSI_GREEN);

                int option_selected = -1;
                try {
                    option = stdIn.readLine();
                    option_selected = Integer.parseInt(option);
                } catch (NumberFormatException e) {
                    GUI.error("number_option_invalid");
                    continue;
                }

                String username, password, residencia, response, subscription, position;

                switch (option_selected) {

                    // login
                    case 0:

                        GUI.warning_no_nl("username: ");
                        username = stdIn.readLine();
                        GUI.warning_no_nl("password: ");
                        password = stdIn.readLine();

                        response = API.login_frontend(username, password);

                        if (response.startsWith("OK")) {

                            GUI.success(response);

                            GUI.warning_no_nl("position (ex. 5 10): ");
                            position = stdIn.readLine();

                            Position readPosition = Tools.get_position_from_string(position, " ");

                            if (readPosition == null) {
                                GUI.error("ERROR:invalid_position");
                                break;
                            } else
                                GUI.success("OK:position_syntax_valid");

                            response = API.register_backend(username, readPosition.getPosX(), readPosition.getPosY());

                            if (response.startsWith("OK")) {

                                GUI.success(response);

                                User userLogged = new User(username, readPosition);

                                process_operations(stdIn, userLogged);

                            } else
                                GUI.error(response);

                        } else
                            GUI.error(response);

                        break;

                    // register
                    case 1:

                        GUI.warning_no_nl("username: ");
                        username = stdIn.readLine();
                        GUI.warning_no_nl("password: ");
                        password = stdIn.readLine();
                        GUI.warning_no_nl("residencia: ");
                        residencia = stdIn.readLine();

                        response = API.register_frontend(username, password, residencia);

                        if (response.startsWith("OK"))
                            GUI.success(response);
                        else
                            GUI.error(response);

                        break;

                    // subscribe
                    case 2:

                        GUI.warning_no_nl("district: ");
                        subscription = stdIn.readLine();

                        response = API.subscribeDistrict(subscription);

                        if (response.startsWith("OK"))
                            GUI.success(response);
                        else
                            GUI.error(response);

                        break;

                    case 3:

                        GUI.success("not implemented yet");
                        // process_diretorio();

                        break;

                    // clear terminal
                    case 4:
                        GUI.clear_terminal();
                        GUI.main_menu("startup", null);
                        continue;

                    default:

                        GUI.error("number_option_invalid");

                        continue;
                }
            }

            socket.close();

        } catch (Exception e) {

            e.printStackTrace();
        }

        System.out.println("[Client:frontend] app stoped...");
    }

    public static void process_operations(BufferedReader stdIn, User user) throws Exception {

        String option = "";

        GUI.clear_terminal();
        GUI.main_menu("operations", user);

        boolean stop_application = false;

        while (!stop_application) {

            GUI.command_prompt("cmd", GUI.ANSI_GREEN);

            int option_selected = -1;
            try {
                option = stdIn.readLine();
                option_selected = Integer.parseInt(option);
            } catch (NumberFormatException e) {
                GUI.error("number_option_invalid");
                continue;
            }

            switch (option_selected) {

                // subscribe
                case 0:
                    GUI.success("not implemented yet");
                    break;

                // update position
                case 1:
                    GUI.success("not implemented yet");
                    break;

                // report infection
                case 2:
                    GUI.success("not implemented yet");
                    break;

                // number of users in location
                case 3:
                    GUI.success("not implemented yet");
                    break;

                // clear terminal
                case 4:
                    GUI.clear_terminal();
                    GUI.main_menu("operations", user);
                    continue;

                default:

                    GUI.error("number_option_invalid");

                    continue;
            }
        }

        // return this function when logout
    }

    public static void process_diretorio() {
        //
    }
}