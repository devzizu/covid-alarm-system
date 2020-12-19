package diretorio.dataTypes;

public class User {
    public String username;
    public boolean contactInf;
    public boolean infetado;

    public User(String username){
        this.username = username;
        this.contactInf = false;
        this.infetado = false;
    }
}
