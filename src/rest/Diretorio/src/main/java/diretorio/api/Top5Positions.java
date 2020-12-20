package diretorio.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Top5Positions implements Comparable<Top5Positions>{
    public int positionX;
    public int positionY;
    public int record;
    public String district;

    @JsonCreator
    public Top5Positions (@JsonProperty("positionX") int posX,@JsonProperty("positionY") int posY,
                          @JsonProperty("record") int rec,@JsonProperty("district") String dist){            
            this.positionX = posX;
            this.positionY = posY;
            this.record = rec;
            this.district = dist;
    }

    @Override
    public int compareTo(Top5Positions p){
        if(this.record >= p.record) return -1;
        return 1;
    }
}
