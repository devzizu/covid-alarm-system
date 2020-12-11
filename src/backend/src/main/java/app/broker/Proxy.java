
package app.broker;

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
		if (items.pollin(0))  { from = s1; to = s2; }
		else { from = s2; to = s1; }

		String acumMessage = "";
		
		while (true) {

			//maybe part of the full message
			byte[] readBytes = from.recv();

			String messageStr = new String(readBytes);

			System.out.println("[PROXY]: read more = " + messageStr);

			//message shall start with: ...
			if (from.hasReceiveMore()) {

				acumMessage+=messageStr;

				if (!(acumMessage.charAt(0) == '+'))
					to.sendMore(readBytes);

			} else {

				String[] parts = acumMessage.split("_");
				//update from a new layer1 broker
				if (parts[0].equals("+")) {

					int xpub_port = Integer.parseInt(parts[1]);
					from.connect("tcp://*:" + xpub_port);

				} else to.send(readBytes);

				break;	
			}
		}
    }
  }

}

