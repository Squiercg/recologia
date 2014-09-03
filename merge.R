library(Rcpp)

sourceCpp("merge.cpp",verbose=T)

mergesortC(sample(1:10))



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

dados<-data.frame(Tempo=rep(NA,2000),Algoritimo=rep(c("InsertionSort","MergeSort"),each=1000))

for(i in 1:2000){
    if(i<=1000){
        dados[i,1]<-system.time(insertionsortC(sample(1:1000)))[3]
    }else{
        dados[i,1]<-system.time(mergesortC(sample(1:1000)))[3]
    }
}


boxplot(Tempo~Algoritimo,data=dados)
