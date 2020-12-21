
package app.client;

public class User {

    private String username;
    private Position pos;

    public User(String username, Position pos) {
        this.username = username;
        this.pos = pos;
    }

    public String getUsername() {
        return this.username;
    }

    public Position getPos() {
        return this.pos;
    }

    public void setPos(Position p) {
        this.pos = p;
    }

    @Override
    public String toString() {
        return "{" + " username='" + getUsername() + "'" + ", pos='" + getPos() + "'" + "}";
    }

    @Override
    public int hashCode() {
        return username.hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof User)) {
            return false;
        }
        User user = (User) o;
        return username.equals(user.username);
    }

}