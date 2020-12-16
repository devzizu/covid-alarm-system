package app.central;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import app.broker.Broker;

public class BrokerProtocol implements Serializable {

    private static final long serialVersionUID = 1L;

    private int XPUB_PORT;
    private List<Integer> XSUB_PORTS;

    public BrokerProtocol() {
        this.XPUB_PORT = -1;
        this.XSUB_PORTS = new ArrayList<>();
    }

    public BrokerProtocol(int XPUB_PORT, List<Integer> XSUB_PORTS) {
        this.XPUB_PORT = XPUB_PORT;
        this.XSUB_PORTS = XSUB_PORTS;
    }

    public int getXPUB_PORT() {
        return this.XPUB_PORT;
    }

    public List<Integer> getXSUB_PORTS() {
        return this.XSUB_PORTS;
    }

    public void setPUB_PORT(int PUB_PORT) {
        this.XPUB_PORT = PUB_PORT;
    }

    public void setSUB_PORTs(List<Broker> listLayer1) {
        for (Broker b : listLayer1) {
            XSUB_PORTS.add(b.getXPUB_PORT());
        }
    }

    @Override
    public String toString() {
        return "XPUBS_connect: " + this.getXSUB_PORTS().toString();
    }
}