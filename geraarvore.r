library(igraph)

#Possibilidade 1
plot(graph.formula("Sp1","Sp2","Sp3","Sp4"))
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3","Sp4"),edge.color="black",edge.width=3)
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","Sp4"+-"A2"),edge.color="black",edge.width=3)
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","Sp4"+-"A2","A2"+-"A3","A1"+-"A3"),edge.color="black",edge.width=3)
tkplot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","Sp4"+-"A2","A2"+-"A3","A1"+-"A3"),edge.color="black",edge.width=3)

#Possibilidade 2
plot(graph.formula("Sp1","Sp2","Sp3","Sp4"))
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3","Sp4"),edge.color="black",edge.width=3)
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","A1"+-"A2","Sp4"),edge.color="black",edge.width=3)
plot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","A1"+-"A2","Sp4"+-"A3","A2"+-"A3"),edge.color="black",edge.width=3)
tkplot(graph.formula("Sp1"+-"A1","Sp2"+-"A1","Sp3"+-"A2","A1"+-"A2","Sp4"+-"A3","A2"+-"A3"),edge.color="black",edge.width=3)


checa_lista<-function(lista,vetor) {
    if(length(lista)==0) {
        return(FALSE)
    } else {
        if(sum(sapply(lapply(lista,function(x) { vetor%in%x })   ,prod))>0) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
}
teste1<-list()
teste2<-list(c(1,2),c(1,3),c(2,3))

checa_lista(teste1,c(2,1))
checa_lista(teste2,c(2,1))

todas_arvores<-function(entrada) {
    saida<-list(entrada)
    while(length(saida[[1]])!=1) {
        aux<-list()
        for(i in 1:length(saida)) {
            temporario<-list()
            combinacoes<-combn(saida[[i]],2)
            for(j in 1:ncol(combinacoes)) {
                tempo<-c(saida[[i]][!saida[[i]]%in%combinacoes[,j]],paste("(",combinacoes[1,j],",",combinacoes[2,j],")",sep=""))
                if(!(checa_lista(aux,tempo)||checa_lista(temporario,tempo))) {
                    temporario<-c(temporario,list(tempo))
                }
            }
            aux<-c(aux,temporario)
        }
        saida<-aux
    }
    saida<-unlist(saida)
    saida<-sapply(saida,function(arvore) {paste(arvore,";",sep="")},USE.NAMES = FALSE)
    return(saida)
}

arvores<-todas_arvores(paste("sp",1:5))
arvores

library(ape)

raiz<-sqrt(length(arvores))
par(mar=c(2,1,2,2))
layout(matrix(1:c(ceiling(raiz)*ceiling(raiz)),nrow=ceiling(raiz),ncol=ceiling(raiz),byrow=T))

for(i in 1:length(arvores)) {
    plot(read.tree(text=arvores[i]),cex=1)
    title(paste("Árvore",i))
}

numero_arvores<-function(n) {
    n<-n+1
    saida<-(factorial(2*n-4))/(factorial(n-2)*2^(n-2))
    return ( saida )
}


n<-vector()

for(i in 2:8) {
    n[i-2]<-numero_arvores(i)
}

plot(n~c(3:8),xlab="Número de espécies",ylab="Árvores possíveis",pch=19,cex=1.2,frame=F)

format(numero_arvores(30),scientific=F)
