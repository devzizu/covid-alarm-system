package app.district;

import java.util.HashSet;

public class UsersInPosition {

    private HashSet<String> USERS_IN_POSITION = new HashSet<>();

    private boolean SENT_LIMIT_NOTIFICATION;

    public UsersInPosition() {
        this.USERS_IN_POSITION = new HashSet<>();
        this.SENT_LIMIT_NOTIFICATION = false;
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

    public boolean getSentLimit() {
        return this.SENT_LIMIT_NOTIFICATION;
    }

    public void setSentLimit(boolean sl) {
        this.SENT_LIMIT_NOTIFICATION = sl;
    }
}
