package diretorio.resources;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.stream.Collectors;

import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.fasterxml.jackson.annotation.JsonProperty;

import diretorio.api.ContactAvg;
import diretorio.api.InfRatio;
import diretorio.api.InfUsers;
import diretorio.api.NUsers;
import diretorio.api.Top5Positions;
import diretorio.dataTypes.*;

@Path("/districts")
@Produces(MediaType.APPLICATION_JSON)
public class DiretorioResource {

    // private final String template;
    private volatile Map<String, Distrito> distritos;

    public DiretorioResource(String template, List<String> districts) {
        // this.template = template;
        this.distritos = new HashMap<>();
        for (String d : districts) {
            distritos.put(d, new Distrito(d));
        }
    }

    // Registo de um utilizador
    @POST
    @Path("/user/reg")
    public Response register(UserClass user) {
        Distrito d = distritos.get(user.district);

        if (d == null)
            return Response.status(400).build();
        boolean b;
        synchronized (this) {
            if (!(b = d.users.containsKey(user.username)))
                d.users.put(user.username, new User(user.username));
        }
        if (!b)
            return Response.status(201).build();
        else
            return Response.status(412).build();// verificar se e o suposto dar conflict
    }

    public static class UserClass {
        @JsonProperty("district")
        public String district;
        @JsonProperty("username")
        public String username;
        @JsonProperty("positionX")
        public int positionX;
        @JsonProperty("positionY")
        public int positionY;
    }

    @PUT
    @Path("/user/infection")
    public Response infection(UserClass user) {
        Distrito d = distritos.get(user.district);
        if (d == null)
            return Response.status(400).build();
        if (d.infectedUser(user.username))
            return Response.ok().build();
        return Response.status(304).build();
    }

    @PUT
    @Path("/user/contact")
    public Response contact(UserClass user) {
        Distrito d = distritos.get(user.district);
        if (d == null)
            return Response.status(400).build();
        if (d.infContactUser(user.username))
            return Response.ok().build();
        return Response.status(304).build();
    }

    @PUT
    @Path("/user/contacts")
    public Response contacts(LUsers users) {
        Distrito d = distritos.get(users.district);
        if (d == null)
            return Response.status(400).build();
        if (d.infContactLUsers(users.users))
            return Response.ok().build();
        return Response.status(304).build();

    }

    public static class LUsers {
        @JsonProperty("district")
        public String district;
        @JsonProperty("users")
        public List<String> users;
    }

    @PUT
    @Path("/top5")
    public Response top5(TopClass pos) {
        Distrito d = distritos.get(pos.district);
        if (d == null)
            return Response.status(400).build();
        if (d.updateTop(pos.positionX, pos.positionY, pos.record))
            return Response.ok().build();
        return Response.status(304).build();
    }

    public static class TopClass {
        @JsonProperty("district")
        public String district;
        @JsonProperty("positionX")
        public int positionX;
        @JsonProperty("positionY")
        public int positionY;
        @JsonProperty("record")
        public int record;
    }

    @GET
    @Path("/users")
    public Response nUsers(@QueryParam("district") String distrito) {
        Distrito d = distritos.get(distrito);
        if (d == null)
            return Response.status(400).build();
        return Response.ok(new NUsers(d.users.size())).build();
    }

    @GET
    @Path("/user")
    public Response userList() {
        return Response
                .ok(distritos.values().stream().flatMap(d -> d.users.keySet().stream()).collect(Collectors.toList()))
                .build();
    }

    @GET
    @Path("/infectedUsers")
    public Response infectedUsers(@QueryParam("district") String distrito) {
        Distrito d = distritos.get(distrito);
        if (d == null)
            return Response.status(400).build();
        return Response.ok(new InfUsers(d.nInfetados)).build();
    }

    @GET
    @Path("/top5InfRatio")
    public Response infectedRatio() {
        return Response.ok(new TreeSet<>(this.distritos.values()).stream()
                .map(e -> new InfRatio(e.name, e.infectedRatio())).limit(5).collect(Collectors.toList())).build();
    }

    @GET
    @Path("/contactAvg")
    public Response contact() {
        return Response
                .ok(new ContactAvg(this.distritos.values().stream().mapToDouble(e -> e.mediaContactos()).sum() / 18.0))
                .build();
    }

    @GET
    @Path("/top5Locations")
    public Response top5Locations() {
        return Response.ok(distritos.values().stream().flatMap(l -> l.top5.stream().map(e -> e.toTop5()))
                .collect(Collectors.toCollection(() -> new TreeSet<Top5Positions>())).stream().limit(5)
                .collect(Collectors.toList())).build();
    }
}
