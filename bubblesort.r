library(Rcpp)

cppFunction("
    NumericVector bubblesortC(NumericVector vetor) {
        int n = vetor.size();

        int i , j , aux;

        for(i=0;i<n-1;i++) {
            for(j=0;j<n-i-1;j++) {
                if(vetor[j]>vetor[j+1]) {
                    aux=vetor[j];
                    vetor[j]=vetor[j+1];
                    vetor[j+1]=aux;
                    }
                }
            }
        return vetor;
        }
")

bubblesortR<-function(vetor) {
    aux<-numeric()
    for(i in 1:(length(vetor)-1)) {
        for(j in 1:(length(vetor)-i)) {
            if(vetor[j]>vetor[j+1]) {
                aux<-vetor[j]
                vetor[j]<-vetor[j+1]
                vetor[j+1]<-aux
                    }
                }
            }
        return(vetor)

    }

vetor<-sample(2000)

bubblesortR(vetor)
bubblesortC(vetor)





