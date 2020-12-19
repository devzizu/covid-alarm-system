package app;

public class DistrictData {

    private int router_port;
    private int pub_port;

    public DistrictData(int router, int pub) {
        this.router_port = router;
        this.pub_port = pub;
    }

    public int getRouterPort() {
        return this.router_port;
    }

    public int getPubPort() {
        return this.pub_port;
    }

    @Override
    public String toString() {
        return "{" + " router.port='" + this.router_port + "'" + ", xsub.conn='" + this.pub_port + "'" + "}";
    }

}