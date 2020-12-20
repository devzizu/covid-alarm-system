package app.district;

import java.util.HashSet;

public class UsersInPosition {

    private HashSet<String> USERS_IN_POSITION = new HashSet<>();

    public UsersInPosition() {
        this.USERS_IN_POSITION = new HashSet<>();
    }

    public void addUser(String uname) {

        this.USERS_IN_POSITION.add(uname);
    }

    public void removeUser(String uname) {
        this.USERS_IN_POSITION.remove(uname);
    }

    public HashSet<String> getUsersInPosition() {
        return this.USERS_IN_POSITION;
    }

    public int getNumberOfUsers() {
        return this.USERS_IN_POSITION.size();
    }
}
