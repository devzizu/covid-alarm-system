package diretorio.dataTypes;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;




public class Distrito implements Comparable<Distrito>{
    
    public String name;
    public int nInfetados;
    public int nContact;
  
    public Map<String,User> users;
    public TreeSet<Posicao> top5;

    public Distrito(String name) {
        this.name = name;
        this.nInfetados=0;
        this.nContact=0;
        this.top5 = new TreeSet<>();
        
        this.users = new ConcurrentHashMap<>();
    }

    public double infectedRatio(){
        double nUsers = users.size();
        if(nUsers==0) return 0;
        return (((double)nInfetados)/nUsers)*100;
    }

    public boolean infectedUser(String username){
        User u = users.get(username);
        if(u==null) return false;
        u.infetado = true;
        this.nInfetados++;
        return true;
    }

    public boolean infContactUser(String username){
        User u = users.get(username);
        if(u==null) return false;
        if (!u.contactInf){
            u.contactInf = true;
            this.nContact++;
        }
        return true;
    }

    public boolean updateTop(int positionX,int positionY,int record){

        Posicao p = new Posicao(positionX, positionY, record, this.name);

        Iterator<Posicao> iterator = top5.iterator();

        while(iterator.hasNext()){
            Posicao p1 = iterator.next();
            if(p1.equals(p)){
                if(p1.record < record){
                    top5.remove(p1);
                    top5.add(p);
                    return true;
                }
                return false;
            }
        }

        top5.add(p);

        if(top5.size()>5){
            top5.pollLast();
        }

        System.out.println(top5.toString());


        return true;
    }

    @Override
    public int compareTo(Distrito d) {
        if(this.infectedRatio() >= d.infectedRatio()) return -1;
        return 1;
    }

    


    public double mediaContactos(){
        double nUsers = users.size();
        if (nUsers==0) return 0;
        return (nContact/nUsers);
    }

}
