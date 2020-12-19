package diretorio.dataTypes;

public class Mapa {

    public static class Pos{
        int atual;
        int max;
    }

    public int [][] posicoes; //o inteiro representa o numero de users na posição

    public Mapa(int N) {
        this.posicoes = new int[N][N];
        for(int i =0;i<N;i++){
            for(int j=0;j<N;j++){
                this.posicoes[i][j] = 0;
            }
        }
    }
}
