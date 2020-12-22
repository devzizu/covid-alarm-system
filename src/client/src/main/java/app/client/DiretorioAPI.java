package app.client;

import java.net.http.*;

import app.ConfigReader;

import app.api.*;

import java.net.*;

import com.fasterxml.jackson.annotation.JsonProperty;

import com.fasterxml.jackson.databind.*;

public class DiretorioAPI {

    ConfigReader config = new ConfigReader();

    HttpClient httpclient = HttpClient.newHttpClient();

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

    public String getNInfected(String district) throws Exception {

        String uri = config.getDiretorioURI() + "/infectedUsers?district=" + district;

        return genericRequest(uri);

    }

    public String getTop5InfRatio() throws Exception {
        String uri = config.getDiretorioURI() + "/top5InfRatio";

        return genericRequest(uri);

    }

    public String getTop5Locations() throws Exception {
        String uri = config.getDiretorioURI() + "/top5Locations";

        return genericRequest(uri);

    }

    public String getContactAvg() throws Exception {
        String uri = config.getDiretorioURI() + "/contactAvg";

        return genericRequest(uri);

    }

    public String genericRequest(String uri) throws Exception {

        StringBuilder result = new StringBuilder();

        HttpRequest get = HttpRequest.newBuilder().uri(URI.create(uri)).GET().build();

        HttpResponse<String> response = this.httpclient.send(get, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() == 200)
            result.append("OK_:").append(response.body());
        else
            result.append("status_code: ").append(response.statusCode()).append("body: ").append(response.body());

        return result.toString();

    }
}