package app.broker;

public class Broker {
   
    private int XPUB_PORT;
    private int XSUB_PORT;

    public Broker(int XPUB_PORT, int XSUB_PORT) {
        this.XPUB_PORT = XPUB_PORT;
        this.XSUB_PORT = XSUB_PORT;
    }

    public int getXPUB_PORT() {
        return this.XPUB_PORT;
    }

    public int getXSUB_PORT() {
        return this.XSUB_PORT;
    }

    @Override
    public String toString() {
        return "{" +
            " XPUB='" + getXPUB_PORT() + "'" +
            ", XSUB='" + getXSUB_PORT() + "'" +
            "}";
    }
}