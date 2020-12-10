package app.central;

import java.util.List;

import app.*;

public class RoundRobinDistrict {

    private int current_server;
    private List<DistrictData> district_servers;  

    public RoundRobinDistrict(List<DistrictData> district_servers) {
        this.current_server = 0;
        this.district_servers = district_servers;
    }

    public int getCurrent_server() {
        return this.current_server;
    }

    public List<DistrictData> getDistrict_servers() {
        return this.district_servers;
    }

    public int sizeL(){
        return this.district_servers.size();
    }

    public DistrictData getNextDistrictServer() {
        this.current_server = (this.current_server+1)%district_servers.size();
        return district_servers.get(this.current_server);
    }

    public void appendDistrictServer(DistrictData d) {
        this.district_servers.add(d);
    }


    @Override
    public String toString() {
        return "{" +
            ", servers='" + getDistrict_servers() + "'" +
            "}";
    }
}