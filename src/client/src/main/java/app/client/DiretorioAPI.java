package app.client;

import java.net.http.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import app.ConfigReader;

import app.api.*;

import java.net.*;

import com.fasterxml.jackson.databind.*;

public class DiretorioAPI {

    private static ConfigReader config = new ConfigReader();

    private static HttpClient httpclient = HttpClient.newHttpClient();

    public int getNUsersDistrict(String district) throws Exception {

        int result = -1;

        String uri = config.getDiretorioURI() + "/users?district=" + district;

        String response = genericRequest(uri);

        if (response.startsWith("OK")) {
            String fields[] = response.split("_:");

            String body = fields[1];

            ObjectMapper om = new ObjectMapper();

            NUsers r = om.readValue(body, NUsers.class);

            result = r.nUsers;

        }

        return result;
    }

    public int getNInfected(String district) throws Exception {

        int result = -1;

        String uri = config.getDiretorioURI() + "/infectedUsers?district=" + district;

        String response = genericRequest(uri);

        if (response.startsWith("OK")) {
            String fields[] = response.split("_:");

            String body = fields[1];

            ObjectMapper om = new ObjectMapper();

            InfUsers r = om.readValue(body, InfUsers.class);

            result = r.infectedUsers;

        }

        return result;

    }

    public List<InfRatio> getTop5InfRatio() throws Exception {

        List<InfRatio> result = null;

        String uri = config.getDiretorioURI() + "/top5InfRatio";

        String response = genericRequest(uri);

        if (response.startsWith("OK")) {
            String fields[] = response.split("_:");

            String body = fields[1];

            ObjectMapper om = new ObjectMapper();

            InfRatio[] resultAux = om.readValue(body, InfRatio[].class);

            result = new ArrayList<InfRatio>(Arrays.asList(resultAux));

        }

        return result;

    }

    public List<Top5Positions> getTop5Locations() throws Exception {
        String uri = config.getDiretorioURI() + "/top5Locations";

        List<Top5Positions> result = null;

        String response = genericRequest(uri);

        if (response.startsWith("OK")) {
            String fields[] = response.split("_:");

            String body = fields[1];

            ObjectMapper om = new ObjectMapper();

            Top5Positions[] resultAux = om.readValue(body, Top5Positions[].class);

            result = new ArrayList<Top5Positions>(Arrays.asList(resultAux));

        }

        return result;

    }

    public double getContactAvg() throws Exception {
        String uri = config.getDiretorioURI() + "/contactAvg";

        double result = -1;

        String response = genericRequest(uri);

        if (response.startsWith("OK")) {
            String fields[] = response.split("_:");

            String body = fields[1];

            ObjectMapper om = new ObjectMapper();

            ContactAvg r = om.readValue(body, ContactAvg.class);

            result = r.contactAvg;

        }

        return result;

    }

    public static String genericRequest(String uri) throws Exception {

        StringBuilder result = new StringBuilder();

        HttpRequest get = HttpRequest.newBuilder().uri(URI.create(uri)).GET().build();

        HttpResponse<String> response = httpclient.send(get, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() == 200)
            result.append("OK_:").append(response.body());
        else
            result.append("status_code: ").append(response.statusCode()).append("body: ").append(response.body());

        return result.toString();

    }
}