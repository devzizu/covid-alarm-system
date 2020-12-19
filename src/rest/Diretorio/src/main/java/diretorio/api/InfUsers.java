package diretorio.api;


import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class InfUsers {
    public final int infectedUsers;

    @JsonCreator
    public InfUsers(@JsonProperty("infectedUsers") int n) {
      this.infectedUsers = n;
    }
}
