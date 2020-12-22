
package app.client.gui;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import app.client.User;

public class GUI {

    // --------------------------------------------------------------------------

    // default ansi colors

    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";

    // banner files
    private static String MAIN_MENU_BANNER = "banners/main_menu";

    // --------------------------------------------------------------------------

    private static List<String> STARTUP_OPTIONS = new ArrayList<>(
            Arrays.asList("login", "register", "subscribe", "diretorio", "clear terminal"));

    private static List<String> OPERATIONS_OPTIONS = new ArrayList<>(Arrays.asList("subscribe", "update position",
            "report infection", "number of users in location", "diretorio", "clear terminal"));

    private static List<String> DIRETORIO_OPTIONS = new ArrayList<>(
            Arrays.asList("number of users", "number of infected users", "top 5 districts (infected ratio)",
                    "top 5 districts (number of users)", "users contact average"));

    // --------------------------------------------------------------------------

    public static void clear_terminal() {
        System.out.print("\033[H\033[2J");
    }

    public static void display_banner_file(String file_name, String color) {

        try {

            BufferedReader reader = new BufferedReader(new FileReader(file_name));

            String line;
            while ((line = reader.readLine()) != null)
                System.out.println(color + line + ANSI_RESET);

            reader.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void error(String error_message) {

        System.out.println(ANSI_RED + "error: " + error_message + ANSI_RESET);
    }

    public static void success(String succString) {

        System.out.println(ANSI_GREEN + succString + ANSI_RESET);
    }

    public static void warning_no_nl(String msg) {

        System.out.print(ANSI_YELLOW + msg + ANSI_RESET);
    }

    public static void warning_nl(String msg) {

        System.out.println(ANSI_YELLOW + msg + ANSI_RESET);
    }

    public static void command_prompt(String tty, String color) {
        System.out.print(color + tty + "> " + ANSI_RESET);
    }

    public static void show_users_in_location(int value) {
        System.out.println(ANSI_WHITE + "There are " + ANSI_RESET + ANSI_RED + value + ANSI_RESET + ANSI_WHITE
                + " users in that location." + ANSI_RESET);
    }

    public static void user_stats(User user) {

        String uname = ANSI_GREEN + "username: " + ANSI_RESET + ANSI_RED + user.getUsername() + ANSI_RESET;
        String locatedX = ANSI_RED + user.getPos().getPosX() + ANSI_RESET;
        String locatedY = ANSI_RED + user.getPos().getPosY() + ANSI_RESET;

        System.out.println(uname + " | " + ANSI_GREEN + "located @ {" + ANSI_RESET + locatedX + "," + locatedY
                + ANSI_GREEN + "}" + ANSI_RESET + "(clear terminal to update information)");
    }

    public static void options(List<String> options, String color) {

        for (int i = 0; i < options.size(); i++) {
            System.out.println(color + "[" + i + "] " + (options.get(i)) + ANSI_RESET);
        }
    }

    public static void main_menu(String type, User user) {

        display_banner_file(MAIN_MENU_BANNER, ANSI_CYAN);

        System.out.println();

        switch (type) {
            case "startup":
                options(STARTUP_OPTIONS, ANSI_WHITE);
                break;
            case "operations":
                user_stats(user);
                System.out.println();
                options(OPERATIONS_OPTIONS, ANSI_WHITE);
                break;
            case "diretorio":
                options(DIRETORIO_OPTIONS, ANSI_WHITE);
                break;
        }

        System.out.println();
    }
}