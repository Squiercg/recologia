n<-10
k<-3

vetor<-c(1,1)

for(i in 3:n) {
    if(i < k+1){
        vetor[i] = vetor[i-1]+vetor[i-2]
    } else{
        if(i == k+1) {
            vetor[i]=vetor[i-1]+vetor[i-2]-vetor[i-k]
        } else {
            vetor[i]=vetor[i-1]+vetor[i-2]-vetor[i-k-1]
        }
    }
}

vetor

fibd<-function(n,k) {
    vetor<-c(1,1)

    for(i in 3:n) {
        if(i < k+1){
            vetor[i] = vetor[i-1]+vetor[i-2]
            }else{
                if(i == k+1) {
                    vetor[i]=vetor[i-1]+vetor[i-2]-vetor[i-k]
                    }else{
                        vetor[i]=vetor[i-1]+vetor[i-2]-vetor[i-k-1]
                    }
            }
    }
    return(vetor)
}


fibd(10,3)
fibd(10,4)
fibd(10,5)
fibd(10,6)
fibd(10,7)
