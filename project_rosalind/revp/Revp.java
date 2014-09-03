import java.io.FileNotFoundException;
import java.util.Scanner;

public class Revp{
    public static void main(String[] args) {

	try {
	    File arquivo = new File("teste01.txt");
	    Scanner entrada = new Scanner(arquivo);
	    n = entrada.nextInt();
	    vetor = new int[n];
	    for(int i=0; i<n ; i++) {
				vetor[i]=entrada.nextInt();
	    }
	    entrada.close();
	} catch (FileNotFoundException e) {
	    System.out.println("Arquivo não encontrado");
        }

    }
}

