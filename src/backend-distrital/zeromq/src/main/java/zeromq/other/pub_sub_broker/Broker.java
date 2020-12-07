package zeromq.pub_sub_broker;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Broker {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket pubs = context.createSocket(SocketType.XSUB);
             ZMQ.Socket subs = context.createSocket(SocketType.XPUB))
        {
            pubs.bind("tcp://*:"+args[0]);
            subs.bind("tcp://*:"+args[1]);
            new Proxy(context, pubs, subs).poll();
        }
  }
}
