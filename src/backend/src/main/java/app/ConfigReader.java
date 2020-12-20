
package app;

import com.moandjiezana.toml.Toml;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class ConfigReader {

    Map<String, Object> portas = new HashMap<String, Object>();
    Map<String, Object> distritos = new HashMap<String, Object>();
    Map<String, Object> def = new HashMap<String, Object>();

    public ConfigReader() {

        File myObj = new File("../config.toml");

        Toml toml = new Toml().read(myObj);

        this.portas = toml.getTable("ports").toMap();
        this.distritos = toml.getTable("local").toMap();
        this.def = toml.getTable("default").toMap();
    }

    public String getPort(String key_principal, String key_secundaria) {

        String port = null;
        if (key_principal.equals("ports")) {

            port = (String) portas.get("\"" + key_secundaria + "\"").toString();

        } else if (key_principal.equals("local")) {

            port = (String) distritos.get("\"" + key_secundaria + "\"").toString();
        }
        return port;
    }

    public String getDistricts() {

        String n = null;
        n = (String) def.get("\"NR_DISTRICTS\"").toString();
        return n;
    }

    public String getWorkers() {

        String n = null;
        n = (String) def.get("\"NR_WORKERS\"").toString();
        return n;
    }

    public int getMapSize() {

        String n = null;
        n = (String) def.get("\"MAP_SIZE\"").toString();
        return Integer.parseInt(n);
    }

    public String getDiretorioURI() {

        String n = null;
        n = (String) def.get("\"DIRETORIO_RESOURCE\"").toString();
        return n;
    }
}
