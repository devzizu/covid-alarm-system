package zeromq.request_reply_broker;

import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Proxy {
  private ZMQ.Poller items;
  private ZMQ.Socket s1, s2;

  Proxy(ZContext context, ZMQ.Socket s1, ZMQ.Socket s2) {
    this.s1 = s1;
    this.s2 = s2;
    items = context.createPoller(2);
    items.register(s1, ZMQ.Poller.POLLIN);
    items.register(s2, ZMQ.Poller.POLLIN);
  }

  void poll() {
    while (!Thread.currentThread().isInterrupted()) {
      items.poll();
      ZMQ.Socket from, to;
      if (items.pollin(0)) {
        from = s1; to = s2;
        System.out.println("From s1");
      } else {
        from = s2; to = s1;
        System.out.println("From s2");
      }
      while (true) {
        byte[] m = from.recv();
        System.out.println(new String(m));
        if (from.hasReceiveMore()) {
          to.sendMore(m);
        } else {
          to.send(m);
          break;
        }
      }
    }
  }

}

