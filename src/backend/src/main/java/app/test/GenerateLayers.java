package app.test;

import app.broker.DynamicBrokerLayer1;
import app.broker.DynamicBrokerLayer2;

public class GenerateLayers {
    public static void main(String[] args) throws InterruptedException {

        int numBrokersLayer1 = Integer.parseInt(args[0]);
        int numBrokersLayer2 = Integer.parseInt(args[1]);

        for(int i = 0; i < numBrokersLayer1; i++){
            new Thread(){
                public void run(){
                DynamicBrokerLayer1.main(new String[0]);
                }
            }.start();
        }
        
        Thread.sleep(1000);
        
        for(int i = 0; i < numBrokersLayer2; i++){
            new Thread(){
                public void run(){
                DynamicBrokerLayer2.main(new String[0]);
                }
            }.start();
        }
        
    }
}
