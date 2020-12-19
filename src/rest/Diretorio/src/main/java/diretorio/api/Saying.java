package diretorio.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Saying {

    public final long id;
    public final String content;

    @JsonCreator
    public Saying(@JsonProperty("id") long id, @JsonProperty("content") String content) {
      this.id = id;
      this.content = content;
    }
}

