
package app.client.gui;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import app.client.DefaultAPI;
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
    private static String SEPARATOR_BANNER = "banners/separator";
    private static String INFECTED_BANNER = "banners/infected";

    // --------------------------------------------------------------------------

    private static List<String> STARTUP_OPTIONS = new ArrayList<>(
            Arrays.asList("login", "register", "subscribe", "unsubscribe", "diretorio", "clear terminal"));

    private static List<String> OPERATIONS_OPTIONS = new ArrayList<>(Arrays.asList("subscribe", "unsubscribe",
            "update position", "report infection", "number of users in location", "diretorio", "clear terminal"));

    private static List<String> DIRETORIO_OPTIONS = new ArrayList<>(
            Arrays.asList("number of users", "number of infected users", "top 5 districts (infected ratio)",
                    "top 5 positions (number of users)", "users contact average", "clear terminal", "< back"));

    private static List<String> INFECTED_OPTIONS = new ArrayList<>(
            Arrays.asList("subscribe", "unsubscribe", "diretorio", "clear terminal"));

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

    public static void show_users_in_location(int value, String append) {
        System.out.println(ANSI_WHITE + "There are " + ANSI_RESET + ANSI_RED + value + ANSI_RESET + ANSI_WHITE + append
                + " users in that location." + ANSI_RESET);
    }

    public static void user_stats(User user) {

        String uname = ANSI_GREEN + "username: " + ANSI_RESET + ANSI_RED + user.getUsername() + ANSI_RESET;
        String locatedX = ANSI_RED + user.getPos().getPosX() + ANSI_RESET;
        String locatedY = ANSI_RED + user.getPos().getPosY() + ANSI_RESET;

        System.out.println(uname + " | " + ANSI_GREEN + user.district + " @ " + " {" + ANSI_RESET + locatedX + ","
                + locatedY + ANSI_GREEN + "}" + ANSI_RESET + " \n");
    }

    public static void options(List<String> options, String color) {

        for (int i = 0; i < options.size(); i++) {
            String opt = options.get(i);
            if (opt.equals("< back") || opt.equals("clear terminal"))
                System.out.println("[" + i + "] " + ANSI_YELLOW + (options.get(i)) + ANSI_RESET);
            else
                System.out.println(color + "[" + i + "] " + (options.get(i)) + ANSI_RESET);
        }
    }

    public static void display_list(String color, String type, List<String> subs) {

        System.out.print(color + type + " = {");
        for (int i = 0; i < subs.size(); i++) {
            String s = subs.get(i);
            if (i == subs.size() - 1)
                System.out.print(s);
            else
                System.out.print(s + ",");
        }
        System.out.print(color + "}\n");
    }

    public static void main_menu(String type, User user, DefaultAPI DefaultAPI, boolean isInfected) {

        if (type.equals("infected")) {

            warning_nl("\nYou should be isolated now!!!\n");
            display_banner_file(INFECTED_BANNER, ANSI_RED);
            display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);
            display_list(ANSI_CYAN, "subscriptions", DefaultAPI.get_subscriptions_list());
            display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);
            System.out.println();
            options(INFECTED_OPTIONS, ANSI_WHITE);
            System.out.println();

        } else {

            if (!isInfected) {

                display_banner_file(MAIN_MENU_BANNER, ANSI_CYAN);
                System.out.println();
            } else {
                warning_nl("\nYou should be isolated now!!!\n");
                display_banner_file(INFECTED_BANNER, ANSI_RED);
            }

            display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);
            display_list(ANSI_CYAN, "subscriptions", DefaultAPI.get_subscriptions_list());
            display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);

            System.out.println();

            switch (type) {
                case "startup":
                    options(STARTUP_OPTIONS, ANSI_WHITE);
                    break;
                case "operations":
                    user_stats(user);
                    display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);
                    options(OPERATIONS_OPTIONS, ANSI_WHITE);
                    break;
                case "diretorio":
                    options(DIRETORIO_OPTIONS, ANSI_WHITE);
                    System.out.println();
                    break;
                case "infected":
                    options(INFECTED_OPTIONS, ANSI_WHITE);
                    break;
            }
        }

        display_banner_file(SEPARATOR_BANNER, ANSI_PURPLE);
        System.out.println("[clear terminal to update info]\n");
    }
}