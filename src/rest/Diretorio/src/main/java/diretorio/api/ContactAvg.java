package diretorio.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ContactAvg {
    public final double contactAvg;
    

    @JsonCreator
    public ContactAvg(@JsonProperty("contactAvg") double n) {
      this.contactAvg = n;
    }
}