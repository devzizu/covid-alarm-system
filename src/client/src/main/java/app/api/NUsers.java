package app.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class NUsers {
  public final int nUsers;

  @JsonCreator
  public NUsers(@JsonProperty("nUsers") int n) {
    this.nUsers = n;
  }
}
