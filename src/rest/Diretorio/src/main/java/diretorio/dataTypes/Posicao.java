package diretorio.dataTypes;

public class Posicao implements Comparable<Posicao>{

    public int positionX;
    public int positionY;
    public int record;
    public String district;

    Posicao(int x, int y, int rec, String dis){

        this.positionX = x;
        this.positionY = y;
        this.record = rec;
        this.district = dis;
    }

    public int compareTo(Posicao p){
        if(this.record >= p.record) return -1;
        return 1;
    }

    @Override
    public boolean equals(Object p1){
        Posicao p = (Posicao) p1;
        return (p.positionX==this.positionX && p.positionY==this.positionY);
    }


    @Override
    public String toString() {
        return "{" +
            " positionX='" + positionX + "'" +
            ", positionY='" + positionY + "'" +
            ", record='" + record + "'" +
            ", district='" + district + "'" +
            "}";
    }
    

    
}
