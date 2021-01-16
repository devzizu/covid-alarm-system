
package app.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.List;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import app.ConfigReader;
import app.api.InfRatio;
import app.api.Top5Positions;
import app.client.gui.GUI;

public class Client {

    static ConfigReader config = new ConfigReader();
    static int PORT_FRONTEND = Integer.parseInt(config.getPort("ports", "FRONTEND"));

    static FrontendReader FRONTEND_READER = null;
    static Notifications notificationThread = null;
    static DefaultAPI DefaultAPI = null;
    static DiretorioAPI DiretorioAPI = null;

    static User userLogged;

    public static boolean recv_notifications(BufferedReader reader) throws IOException {

        // 1st request, block until it gets a notification socket
        System.out.println("[Client:app] Requesting notification socket...");

        String msg = reader.readLine();
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

        System.out.println("[Client:app] started...");

        try (ZContext context = new ZContext(); ZMQ.Socket PULL_INPROC_SOCKET = context.createSocket(SocketType.PULL)) {

            PULL_INPROC_SOCKET.bind("inproc://push_reader");

            Socket socket = new Socket("localhost", PORT_FRONTEND);
            PrintWriter writer = new PrintWriter(socket.getOutputStream());
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            if (!socket.isConnected()) {

                System.out.println("[Client:app] (error) connecting to frontend...");
                socket.close();
                return;
            }

            System.out.println("[Client:app] connected with frontend...");

            // Request and start notification sub zmq socket
            boolean canReceiveNotifications = recv_notifications(reader);

            FRONTEND_READER = new FrontendReader(context, socket, reader);

            DefaultAPI = new DefaultAPI(PULL_INPROC_SOCKET, reader, writer, notificationThread);
            DiretorioAPI = new DiretorioAPI();

            if (!canReceiveNotifications) {
                socket.close();
                return;
            }

            // read client requests

            // Read from stdin
            BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in));

            String option = "";

            GUI.clear_terminal();
            GUI.main_menu("startup", null, DefaultAPI, false);

            boolean stop_application = false;

            while (!stop_application) {

                GUI.command_prompt("cmd", GUI.ANSI_GREEN);

                int option_selected = -1;
                try {
                    option = stdIn.readLine();
                    option_selected = Integer.parseInt(option);
                } catch (NumberFormatException e) {
                    if (option.length() != 0)
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

                        response = DefaultAPI.login_frontend(username, password);

                        if (response.startsWith("OK")) {

                            GUI.success(response);

                            Position readPosition = null;
                            boolean firstIter = true;

                            do {

                                if (!firstIter)
                                    GUI.error("invalid_position");

                                GUI.warning_no_nl("position (ex. 5 10): ");
                                position = stdIn.readLine();

                                readPosition = Tools.get_position_from_string(position, " ");

                                firstIter = false;

                            } while (readPosition == null);

                            GUI.success("OK:position_syntax_valid");

                            FRONTEND_READER.setUsername(username);
                            FRONTEND_READER.start();

                            response = DefaultAPI.register_backend(username, readPosition.getPosX(),
                                    readPosition.getPosY());

                            if (response.startsWith("OK")) {

                                GUI.success(response);

                                userLogged.setPos(readPosition);
                                userLogged.setUsername(username);
                                process_operations(stdIn, userLogged);

                                stop_application = true;

                                process_infected(stdIn);

                                return;

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

                        response = DefaultAPI.register_frontend(username, password, residencia);

                        if (response.startsWith("OK")) {
                            GUI.success(response);
                            GUI.warning_nl("You need to login now!");
                            String dist = Character.toUpperCase(residencia.charAt(0))
                                    + residencia.substring(1).toLowerCase();
                            userLogged = new User();
                            userLogged.district = dist;
                        } else
                            GUI.error(response);

                        break;

                    // subscribe
                    case 2:

                        GUI.warning_no_nl("district: ");
                        subscription = stdIn.readLine();

                        response = DefaultAPI.subscribeDistrict(subscription);

                        if (response.startsWith("OK"))
                            GUI.success(response);
                        else
                            GUI.error(response);

                        break;

                    // unsubscribe
                    case 3:

                        GUI.warning_no_nl("district: ");
                        subscription = stdIn.readLine();

                        response = DefaultAPI.unsubscribeDistrict(subscription);

                        if (response.startsWith("OK"))
                            GUI.success(response);
                        else
                            GUI.error(response);

                        break;

                    case 4:

                        process_diretorio(stdIn, false);
                        GUI.clear_terminal();
                        GUI.main_menu("startup", null, DefaultAPI, false);

                        break;

                    // clear terminal
                    case 5:
                        GUI.clear_terminal();
                        GUI.main_menu("startup", null, DefaultAPI, false);
                        continue;

                    default:

                        GUI.error("number_option_invalid");

                        continue;
                }
            }

            socket.close();

        } catch (Exception e) {
            System.out.println(e.getMessage());
            // e.printStackTrace();
        }

        System.out.println("[Client] app stoped...");
    }

    public static void process_operations(BufferedReader stdIn, User user) throws Exception {

        String option = "";

        GUI.clear_terminal();
        GUI.main_menu("operations", user, DefaultAPI, false);

        boolean stop_application = false;

        while (!stop_application) {

            GUI.command_prompt("cmd", GUI.ANSI_GREEN);

            int option_selected = -1;
            try {
                option = stdIn.readLine();
                option_selected = Integer.parseInt(option);
            } catch (NumberFormatException e) {
                if (option.length() != 0)
                    GUI.error("number_option_invalid");
                continue;
            }

            Position readPosition = new Position();
            String response, subscription, position;

            switch (option_selected) {

                // subscribe
                case 0:
                    GUI.warning_no_nl("district: ");
                    subscription = stdIn.readLine();

                    response = DefaultAPI.subscribeDistrict(subscription);

                    if (response.startsWith("OK"))
                        GUI.success(response);
                    else
                        GUI.error(response);

                    break;

                case 1:
                    GUI.warning_no_nl("district: ");
                    subscription = stdIn.readLine();

                    response = DefaultAPI.unsubscribeDistrict(subscription);

                    if (response.startsWith("OK"))
                        GUI.success(response);
                    else
                        GUI.error(response);

                    break;

                // update position
                case 2:

                    GUI.warning_no_nl("position (ex.: 5 10): ");
                    position = stdIn.readLine();

                    readPosition = Tools.get_position_from_string(position, " ");

                    if (readPosition == null) {
                        GUI.error("invalid_position");
                        break;
                    } else
                        GUI.success("OK:position_syntax_valid");

                    response = DefaultAPI.update_position_backend(user.getUsername(), readPosition);

                    if (response.startsWith("OK")) {

                        GUI.success(response);
                        user.setPos(readPosition);

                    } else {

                        GUI.error(response);
                        GUI.error("user_position_not_updated");
                    }

                    break;

                // report infection
                case 3:

                    GUI.warning_nl("reporting infection...");

                    response = DefaultAPI.report_infection_backend(user.getUsername());

                    if (response.startsWith("OK")) {

                        notificationThread.unsubscribe(user.getUsername());

                        GUI.success(response);

                        stop_application = true;

                        return;

                    } else {

                        GUI.error("could not report infection...");
                        GUI.error(response);
                    }

                    break;

                // number of users in location
                case 4:

                    GUI.warning_no_nl("position (ex.: 5 10): ");
                    position = stdIn.readLine();

                    readPosition = Tools.get_position_from_string(position, " ");

                    if (readPosition == null) {
                        GUI.error("invalid_position");
                        break;
                    } else
                        GUI.success("OK:position_syntax_valid");

                    int nr_users = DefaultAPI.nrusers_location_backend(user.getUsername(), readPosition);

                    if (nr_users >= 0)
                        GUI.show_users_in_location(nr_users, "");
                    else
                        GUI.error("could_not_request_that_position");

                    break;

                // diretorio
                case 5:

                    process_diretorio(stdIn, false);

                    GUI.clear_terminal();
                    GUI.main_menu("operations", user, DefaultAPI, false);
                    break;

                // clear terminal
                case 6:

                    GUI.clear_terminal();
                    GUI.main_menu("operations", user, DefaultAPI, false);

                    continue;

                default:

                    GUI.error("number_option_invalid");

                    continue;
            }
        }
    }

    public static void process_diretorio(BufferedReader stdIn, boolean infected) throws Exception {

        String option = "";

        GUI.clear_terminal();
        GUI.main_menu("diretorio", null, DefaultAPI, infected);

        boolean stop_application = false;

        while (!stop_application) {

            GUI.command_prompt("cmd", GUI.ANSI_GREEN);

            int option_selected = -1;
            try {
                option = stdIn.readLine();
                option_selected = Integer.parseInt(option);
            } catch (NumberFormatException e) {
                if (option.length() != 0)
                    GUI.error("number_option_invalid");
                continue;
            }

            String distRead, distSanitized;

            switch (option_selected) {

                // number of users
                case 0:

                    GUI.warning_no_nl("district: ");
                    distRead = stdIn.readLine();

                    distSanitized = Character.toUpperCase(distRead.charAt(0)) + distRead.substring(1).toLowerCase();

                    if (config.containsDistrict(distSanitized)) {

                        GUI.warning_nl(
                                "requesting GET / " + config.getDiretorioURI() + "/users?district=" + distSanitized);

                        int nr_users = DiretorioAPI.getNUsersDistrict(distSanitized);

                        if (nr_users == -1) {

                            GUI.error("could_not_make_request");

                        } else {

                            GUI.show_users_in_location(nr_users, "");
                        }

                    } else {

                        GUI.error("invalid_district_specified");

                    }

                    break;

                // number of infected users
                case 1:

                    GUI.warning_no_nl("district: ");
                    distRead = stdIn.readLine();

                    distSanitized = Character.toUpperCase(distRead.charAt(0)) + distRead.substring(1).toLowerCase();

                    if (config.containsDistrict(distSanitized)) {

                        GUI.warning_nl("requesting GET / " + config.getDiretorioURI() + "/infectedUsers?district="
                                + distSanitized);

                        int nr_users = DiretorioAPI.getNInfected(distSanitized);

                        if (nr_users == -1) {

                            GUI.error("could_not_make_request");

                        } else {

                            GUI.show_users_in_location(nr_users, " infected");
                        }

                    } else {

                        GUI.error("invalid_district_specified");
                    }

                    break;

                // top 5 districts (infected ratio)
                case 2:

                    GUI.warning_nl("requesting GET / " + config.getDiretorioURI() + "/top5InfRatio");

                    List<InfRatio> top5infected = DiretorioAPI.getTop5InfRatio();

                    if (top5infected == null) {

                        GUI.error("could_not_make_request");

                    } else {
                        boolean shouldShow = top5infected.stream().anyMatch(t5 -> t5.infectedRatio > 0.0);

                        if (!shouldShow) {

                            GUI.success("there result is an empty list.");
                            break;
                        }

                        int topIndex = 1;
                        for (InfRatio ifr : top5infected) {
                            System.out.println(GUI.ANSI_CYAN + "[#" + topIndex + "]" + GUI.ANSI_RESET + " District: "
                                    + GUI.ANSI_GREEN + ifr.distrito + GUI.ANSI_RESET + ", ratio: " + GUI.ANSI_CYAN
                                    + ifr.infectedRatio + GUI.ANSI_RESET + "%");
                            topIndex++;
                        }
                    }

                    break;

                // top 5 districts (number of users)
                case 3:

                    GUI.warning_nl("requesting GET / " + config.getDiretorioURI() + "/top5Locations");

                    List<Top5Positions> top5Locations = DiretorioAPI.getTop5Locations();

                    if (top5Locations == null) {

                        GUI.error("could_not_make_request");
                        break;

                    } else if (top5Locations.isEmpty()) {

                        GUI.success("there result is an empty list.");
                        break;

                    } else {
                        int topIndex = 1;
                        for (Top5Positions ifr : top5Locations) {
                            System.out.println(GUI.ANSI_CYAN + "[#" + topIndex + "]" + GUI.ANSI_RESET + " District: "
                                    + GUI.ANSI_GREEN + ifr.district + GUI.ANSI_RESET + " has record " + GUI.ANSI_CYAN
                                    + ifr.record + GUI.ANSI_RESET + " @ {" + GUI.ANSI_RED + ifr.positionX
                                    + GUI.ANSI_RESET + "," + GUI.ANSI_RED + +ifr.positionY + GUI.ANSI_RESET + "}.");

                            topIndex++;
                        }
                    }

                    break;

                // users contact average
                case 4:

                    GUI.warning_nl("requesting GET / " + config.getDiretorioURI() + "/contactAvg");

                    double avg = DiretorioAPI.getContactAvg();

                    if (avg == -1) {

                        GUI.error("could_not_make_request");

                    } else {

                        System.out
                                .println("Average global users contact ratio " + GUI.ANSI_CYAN + avg + GUI.ANSI_RESET);
                    }

                    break;

                // clear terminal
                case 5:

                    GUI.clear_terminal();
                    GUI.main_menu("diretorio", null, DefaultAPI, infected);

                    continue;

                // clear terminal
                case 6:

                    return;

                default:

                    GUI.error("number_option_invalid");

                    continue;
            }

        }
    }

    public static void process_infected(BufferedReader stdIn) throws Exception {

        String option = "";

        GUI.clear_terminal();
        GUI.main_menu("infected", null, DefaultAPI, true);

        boolean stop_application = false;

        while (!stop_application) {

            GUI.command_prompt("cmd", GUI.ANSI_GREEN);

            int option_selected = -1;
            try {
                option = stdIn.readLine();
                option_selected = Integer.parseInt(option);
            } catch (NumberFormatException e) {
                if (option.length() != 0)
                    GUI.error("number_option_invalid");
                continue;
            }

            String subscription, response;

            switch (option_selected) {

                // subscribe
                case 0:

                    GUI.warning_no_nl("district: ");
                    subscription = stdIn.readLine();

                    response = DefaultAPI.subscribeDistrict(subscription);

                    if (response.startsWith("OK"))
                        GUI.success(response);
                    else
                        GUI.error(response);

                    break;

                case 1:
                    GUI.warning_no_nl("district: ");
                    subscription = stdIn.readLine();

                    response = DefaultAPI.unsubscribeDistrict(subscription);

                    if (response.startsWith("OK"))
                        GUI.success(response);
                    else
                        GUI.error(response);

                    break;

                // diretorio
                case 2:

                    process_diretorio(stdIn, true);

                    GUI.clear_terminal();
                    GUI.main_menu("infected", null, DefaultAPI, true);

                    break;

                // clear terminal
                case 3:

                    GUI.clear_terminal();
                    GUI.main_menu("infected", null, DefaultAPI, true);

                    continue;

                default:

                    GUI.error("number_option_invalid");

                    continue;
            }

        }
    }
}