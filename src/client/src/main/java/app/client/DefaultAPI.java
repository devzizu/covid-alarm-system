
package app.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashSet;

import app.ConfigReader;

public class DefaultAPI {

    private BufferedReader inputReader;
    private PrintWriter outputWriter;
    private Notifications notificationsThread;
    private HashSet<String> subscriptions;
    private final int MAX_SUBS;
    private final int MAP_SIZE;

    static ConfigReader config = new ConfigReader();

    public DefaultAPI(BufferedReader inputReader, PrintWriter outputWriter, Notifications nthread) {
        this.inputReader = inputReader;
        this.outputWriter = outputWriter;
        this.notificationsThread = nthread;
        this.MAX_SUBS = config.getLimitSubs();
        this.MAP_SIZE = config.getMapSize();
        this.subscriptions = new HashSet<>();
    }

    public String login_frontend(String username, String password) throws IOException {

        StringBuilder sb_request = new StringBuilder();
        sb_request.append("login ").append(username).append(" ").append(password);

        this.outputWriter.println(sb_request.toString());
        this.outputWriter.flush();

        String response = this.inputReader.readLine();
        StringBuilder sb_reply = new StringBuilder();

        if (response.startsWith("{ok")) {

            sb_reply.append("OK:login_successfull");

            this.notificationsThread.subscribe(username);

        } else
            sb_reply.append(response);

        return sb_reply.toString();
    }

    public String register_frontend(String username, String password, String distrito) throws IOException {

        String sanitezed = Character.toUpperCase(distrito.charAt(0)) + distrito.substring(1).toLowerCase();
        StringBuilder sb_reply = new StringBuilder();

        if (config.containsDistrict(sanitezed)) {

            StringBuilder sb_request = new StringBuilder();

            sb_request.append("create ").append(username).append(" ").append(password).append(" ").append(sanitezed);

            this.outputWriter.println(sb_request.toString());
            this.outputWriter.flush();

            String response = this.inputReader.readLine();

            if (response.endsWith("login_pls}"))
                sb_reply.append("OK:frontend_register_successfull");
            else
                sb_reply.append(response);
        } else
            sb_reply.append("invalid_district_specified");

        return sb_reply.toString();
    }

    public String register_backend(String username, int posX, int posY) throws IOException {

        StringBuilder sb_reply = new StringBuilder();

        if (posX < MAP_SIZE && posX > 0 && posY > 0 && posY < MAP_SIZE) {

            StringBuilder sb_request = new StringBuilder();
            sb_request.append(username).append("_registo_").append(posX).append("_").append(posY);

            this.outputWriter.println(sb_request.toString());
            this.outputWriter.flush();

            String response = this.inputReader.readLine();

            if (response.startsWith("OK"))
                sb_reply.append("OK:backend_register_successfull");
            else
                sb_reply.append(response);
        } else
            sb_reply.append("position_invalid");

        return sb_reply.toString();
    }

    public String subscribeDistrict(String district) {

        StringBuilder sb_reply = new StringBuilder();

        if (this.subscriptions.size() <= MAX_SUBS) {

            String sanitezed = Character.toUpperCase(district.charAt(0)) + district.substring(1).toLowerCase();

            if (this.subscriptions.contains(sanitezed)) {

                sb_reply.append("already_subscribed");

            } else {

                if (config.containsDistrict(sanitezed)) {
                    this.notificationsThread.subscribe(sanitezed);
                    sb_reply.append("OK:subscribed_successfully");
                    this.subscriptions.add(sanitezed);
                } else
                    sb_reply.append("invalid_district_specified");
            }

        } else
            sb_reply.append("too_many_subscriptions");

        return sb_reply.toString();
    }

    public String update_position_backend(String username, Position newPosition) throws IOException {

        StringBuilder sb_reply = new StringBuilder();

        int posX = newPosition.getPosX();
        int posY = newPosition.getPosY();

        if (posX < MAP_SIZE && posX > 0 && posY > 0 && posY < MAP_SIZE) {

            StringBuilder sb_request = new StringBuilder();
            sb_request.append(username).append("_track_").append(posX).append("_").append(posY);

            this.outputWriter.println(sb_request.toString());
            this.outputWriter.flush();

            String response = this.inputReader.readLine();

            if (response.startsWith("OK"))
                sb_reply.append("OK:position_updated");
            else
                sb_reply.append(response);

        } else
            sb_reply.append("position_invalid");

        return sb_reply.toString();
    }

    public String report_infection_backend(String username) throws IOException {

        StringBuilder sb_request = new StringBuilder();
        sb_request.append(username).append("_infected");

        this.outputWriter.println(sb_request.toString());
        this.outputWriter.flush();

        String response = this.inputReader.readLine();

        StringBuilder sb_reply = new StringBuilder();

        if (response.startsWith("OK"))
            sb_reply.append("OK:infection_reported_logout");
        else
            sb_reply.append(response);

        return sb_reply.toString();
    }

    public int nrusers_location_backend(String username, Position requestedPosition) throws IOException {

        StringBuilder sb_request = new StringBuilder();

        int posX = requestedPosition.getPosX();
        int posY = requestedPosition.getPosY();

        sb_request.append(username).append("_n-users-in-pos_").append(posX).append("_").append(posY);

        this.outputWriter.println(sb_request.toString());
        this.outputWriter.flush();

        String response = this.inputReader.readLine();

        int nr_users = -1;
        if (response.startsWith("OK")) {

            String parts[] = response.split("_");
            nr_users = Integer.parseInt(parts[1]);
        }

        return nr_users;
    }
}