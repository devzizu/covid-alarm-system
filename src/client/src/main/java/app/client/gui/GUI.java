package app.client.gui;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
            Arrays.asList("login", "register", "operation", "clear"));

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

        System.out.println(ANSI_GREEN + "success! " + succString + ANSI_RESET);
    }

    public static void warning_no_nl(String msg) {

        System.out.print(ANSI_BLUE + msg + ANSI_RESET);
    }

    public static void warning_nl(String msg) {

        System.out.println(ANSI_YELLOW + msg + ANSI_RESET);
    }

    public static void command_prompt(String tty, String color) {
        System.out.print(color + tty + "> " + ANSI_RESET);
    }

    public static void options(List<String> options, String color) {

        for (int opt = 0; opt < options.size(); opt++) {
            System.out.println(color + "[" + opt + "] " + options.get(opt) + ANSI_RESET);
        }
    }

    public static void main_menu() {

        display_banner_file(MAIN_MENU_BANNER, ANSI_BLUE);
        System.out.println();
        options(STARTUP_OPTIONS, ANSI_WHITE);
    }
}