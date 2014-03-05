collatz<-function(n) {
    if( (n %% 2) == 0 ) {
        n<-n/2
    } else {
        n<-n*3+1
    }
    return(n)
}

n<-22

while(n!=1) {
    n<-collatz(n)
    print(n)
}

seqcollatz<-function(n) {
    vetor<-c(n)
    i<-1

    while(vetor[i]!=1) {
        vetor[i+1]<-collatz(vetor[i])
        i<-i+1
    }

    return(vetor)
}

seqcollatz(22)
seqcollatz(10)

library(igraph)

#######################################
n<-as.character(seqcollatz(22))
n

grafo<-graph.formula()

grafo<-add.vertices(grafo,length(n))
V(grafo)$name<-n

for(i in 1:c(length(n)-1)) {
    grafo[n[i],n[i+1]]<-TRUE
}

plot(grafo,vertex.size=10)
#########################################
n<-as.character(seqcollatz(8))
n

grafo<-graph.formula()

grafo<-add.vertices(grafo,length(n))
V(grafo)$name<-n

for(i in 1:c(length(n)-1)) {
    grafo[n[i],n[i+1]]<-TRUE
}

plot(grafo,vertex.size=10)



for(i in 2:20) {
    n<-as.character(seqcollatz(i))
    n

    logico<-!n%in%V(grafo)$name

    grafo<-add.vertices(grafo,length(n[logico]))
    V(grafo)$name[is.na(V(grafo)$name)]<-n[logico]

    for(i in 1:c(length(n)-1)) {
        grafo[n[i],n[i+1]]<-TRUE
    }
}

plot(grafo,vertex.size=10)






