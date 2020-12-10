
package app;

import com.moandjiezana.toml.Toml;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class ConfigReader {

    Map<String,Object> portas = new HashMap<String, Object>();
    Map<String,Object> distritos = new HashMap<String, Object>();
    Map<String,Object> defDist = new HashMap<String, Object>();


    public ConfigReader(){

        File myObj = new File("../config.toml");

        Toml toml = new Toml().read(myObj);
        
        this.portas = toml.getTable("ports").toMap();
        this.distritos = toml.getTable("local").toMap();
        this.defDist = toml.getTable("default").toMap();
    }
    public String getPort(String key_principal, String key_secundaria){

        String port = null;
        if(key_principal.equals("ports")){

            port = (String) portas.get("\""+key_secundaria+"\"").toString();

        } else if(key_principal.equals("local")){

            port = (String) distritos.get("\""+key_secundaria+"\"").toString();
        }
        return port;
    }

    public String getNumDistricts(){

        String n = null;
            n = (String) defDist.get("\"NR_DISTRICTS\"").toString();
        return n;
    }


}
