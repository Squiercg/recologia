library(Rcpp)

cppFunction("
    NumericVector bubblesortC(NumericVector vetor) {
        int n = vetor.size();

        double aux;
        int i , j;

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

vetor<-sample(100)

vetor
bubblesortR(vetor)
bubblesortC(vetor)

system.time(bubblesortR(sample(100)))
system.time(bubblesortR(sample(1000)))
system.time(bubblesortR(sample(5000)))

system.time(bubblesortC(sample(100)))
system.time(bubblesortC(sample(1000)))
system.time(bubblesortC(sample(5000)))


vetor<-sample(8)

#Animação
num<-1
aux<-vector()
resposta<-logical()

for(i in 1:(length(vetor)-1)) {
    for(j in 1:(length(vetor)-i)) {
        num<-num+1
        jpeg(sprintf("bubble%04d.jpg",num), width = 350, height = 350)
        cores<-rep("grey",8)
        cores[c(j,j+1)]<-"blue"
        resposta<-vetor[j]>vetor[j+1]
        barplot(vetor,names.arg = vetor,yaxt="n",col=cores,main=paste(vetor[j],"maior que", vetor[j+1],"?"))
        dev.off()

         if(vetor[j]>vetor[j+1]) {
             aux<-vetor[j]
             vetor[j]<-vetor[j+1]
             vetor[j+1]<-aux
             }

        num<-num+1
        jpeg(sprintf("bubble%04d.jpg",num), width = 350, height = 350)
        if(resposta==TRUE) {
            cores<-rep("grey",8)
            cores[c(j,j+1)]<-"red"
            barplot(vetor,names.arg = vetor,yaxt="n",col=cores,main=paste("Trocamos",vetor[j],"e", vetor[j+1],"de lugar"))
            }else{
                cores<-rep("grey",8)
                cores[c(j,j+1)]<-"blue"
                barplot(vetor,names.arg = vetor,yaxt="n",col=cores,main="Não trocamos nada")
                }
        dev.off()
        }
    }

system("convert -delay 90 bu*.jpg  bubblesort.gif")
system("rm bu*.jpg")




