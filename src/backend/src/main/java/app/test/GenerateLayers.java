package app.test;

import app.broker.DynamicBrokerLayer1;
import app.broker.DynamicBrokerLayer2;

public class GenerateLayers {

    static int id = 0;

    public static void main(String[] args) throws InterruptedException {

        int numBrokersLayer1 = Integer.parseInt(args[0]);
        int numBrokersLayer2 = Integer.parseInt(args[1]);

        System.out.println("[Generator:Layers1/2] generating (#layer1=" + numBrokersLayer1 + "; #layer2="
                + numBrokersLayer2 + ")...");

        for (; id < numBrokersLayer1; id++) {
            new Thread() {
                public void run() {
                    System.out.println("[Generator:Layers1/2] (#" + id + ") layer 1 started...");
                    DynamicBrokerLayer1.main(new String[0]);
                }
            }.start();
        }

        System.out.println("[Generator:Layers1/2] waiting...");
        Thread.sleep(1000);

        for (id = 0; id < numBrokersLayer2; id++) {
            new Thread() {

                public void run() {
                    System.out.println("[Generator:Layers1/2] (#" + id + ") layer 2 started...");
                    DynamicBrokerLayer2.main(new String[0]);
                }
            }.start();
        }

        System.out.println("[Generator:Layers1/2] layers generated...");
    }
}