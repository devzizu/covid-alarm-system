package zeromq.chat_service_v2;

import java.util.*;

public class Config {

    static List<Integer> Servers = new ArrayList<>(Arrays.asList(
        10000, 10001, 10002, 10003, 10004, 10005
    ));
    static int BASE_PORT_PULL_DYNAMIC = 20000;
    static int BASE_PORT_XPUB_DYNAMIC = 10100;    
    static int CENTRAL_REQ = 12345;
    static int CENTRAL_PUB = 12346;
}