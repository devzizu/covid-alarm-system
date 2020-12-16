package app;

public class DistrictData {

    private int pull_port;
    private int pub_port;

    public DistrictData(int pull, int pub) {
        this.pull_port = pull;
        this.pub_port = pub;
    }

    public int getPullPort() {
        return this.pull_port;
    }

    public int getPubPort() {
        return this.pub_port;
    }

    @Override
    public String toString() {
        return "{" + " pull='" + this.pull_port + "'" + ", pub='" + this.pub_port + "'" + "}";
    }

}