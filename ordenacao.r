library(Rcpp)

cppFunction("
    NumericVector insertionsortC(NumericVector vetor) {
        int n = vetor.size();

        double aux;
        int i , j;

        for(i=1;i<n;i++) {
            aux=vetor[i];
            j=i-1;
            while(j>=0 && vetor[j]>aux) {
                vetor[j+1]=vetor[j];
                j=j-1;
                }
            vetor[j+1]=aux;
            }
        return vetor;
        }
")


vetor<-sample(100)
vetor
insertionsortC(vetor)

cppFunction("
    NumericVector insertionsortRC(NumericVector vetor, int n) {

        double aux;
        int i;

        if(n>1) {
            insertionsortRC(vetor,n-1);
            aux=vetor[n-1];
            i=n-1;
            while(vetor[i-1]>aux && i>=0 ) {
                vetor[i]=vetor[i-1];
                i--;
                }
            vetor[i]=aux;
            }

        return vetor;
        }
    ")

vetor
insertionsortRC(vetor,length(vetor))



