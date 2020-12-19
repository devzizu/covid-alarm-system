package diretorio;

import io.dropwizard.Configuration;

import javax.validation.constraints.NotEmpty;
import java.util.List;

public class DiretorioConfiguration extends Configuration {
    @NotEmpty
    public String template;

    @NotEmpty
    public List<String> districts;


    public int mapSize;
}
