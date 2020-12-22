package app.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class InfRatio {
  public final String distrito;
  public final double infectedRatio;

  @JsonCreator
  public InfRatio(@JsonProperty("distrito") String d, @JsonProperty("infectedRatio") double n) {
    this.distrito = d;
    this.infectedRatio = n;
  }
}
