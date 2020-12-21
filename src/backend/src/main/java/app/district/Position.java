package app.district;

public class Position {

    private int posX;
    private int posY;

    public Position() {
        this.posX = 0;
        this.posY = 0;
    }

    public Position(int posX, int posY) {
        this.posX = posX;
        this.posY = posY;
    }

    @Override
    public String toString() {
        return "{" + " posX='" + getPosX() + "'" + ", posY='" + getPosY() + "'" + "}";
    }

    public int getPosX() {
        return this.posX;
    }

    public int getPosY() {
        return this.posY;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Position)) {
            return false;
        }
        Position position = (Position) o;
        return posX == position.posX && posY == position.posY;
    }

}
