
package app.client;

public class Tools {

    public static Position get_position_from_string(String input, String separator) {

        // Excepts input as : posX<separator>posY

        Position resultPosition;

        try {

            String parts[] = input.split(separator);

            int posX = Integer.parseInt(parts[0]);
            int posY = Integer.parseInt(parts[1]);

            resultPosition = new Position(posX, posY);

        } catch (Exception e) {

            resultPosition = null;
        }

        return resultPosition;
    }

}