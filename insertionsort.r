vetor<-sample(100)
vetor

insertionsort<-function(vetor){
    n<-length(vetor)

    for(i in 2:n) {
        aux=vetor[i]
        j=i-1
        while(j>=1 && vetor[j]>aux) {
            vetor[j+1]<-vetor[j]
            j=j-1
            }
        vetor[j+1]=aux
        }
    return(vetor)
    }

insertionsort(vetor)

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

fatorial<-function(n) {
    if(n==1) {
        return(1)
        } else {
            return(n*fatorial(n-1))
            }
    }

fatorial(3)

insertionsortR<-function(vetor,n) {

    if(n>1) {
        vetor <- insertionsortR(vetor,n-1) ## mudei o código aqui nessa linha
        aux<-vetor[n]
        i<-n
        while(vetor[i-1]> aux && i > 1) {
            vetor[i]<-vetor[i-1]
            i<- i-1
            }
        vetor[i]<-aux
        }

    return(vetor)
    }

insertionsortR(vetor,length(vetor))



