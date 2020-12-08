import java.io.*;
import java.net.Socket;

public class Client {

    public static void main(String[] args) {

        int port = 12345;

        try {

            Socket socket = new Socket("localhost", port);

            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter writer = new PrintWriter(socket.getOutputStream());
            BufferedReader stdIn = new BufferedReader(new InputStreamReader(System.in));

            String msg = stdIn.readLine();

            writer.println(msg);
            writer.flush();

            String response = reader.readLine();

            System.out.println("A resposta do servidor: " + response);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}