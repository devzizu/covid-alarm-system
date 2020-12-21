package app.district;

import java.util.Objects;

public class Record implements Comparable<Record> {

    private Position pos;
    private int record;

    public Record(Position pos, int record) {
        this.pos = pos;
        this.record = record;
    }

    public Position getPos() {
        return this.pos;
    }

    public int getRecord() {
        return this.record;
    }

    public void setRecord(int r) {
        this.record = r;
    }

    @Override
    public String toString() {
        return "{" + " pos='" + getPos() + "'" + ", record='" + getRecord() + "'" + "}";
    }

    @Override
    public int compareTo(Record r) {
        if (this.pos.equals(r.pos))
            return 0;
        if (this.record >= r.getRecord())
            return -1;
        return 1;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Record)) {
            return false;
        }
        Record record = (Record) o;
        return this.pos.equals(record.pos);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pos, record);
    }

}