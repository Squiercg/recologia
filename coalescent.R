###http://recologia.com.br/2016/02/teoria-da-coalescencia/
library(igraph)

###Definindo a função de probabilidade de coalescencia
coalescent<- function(n,t) {
    return((1/n) * (1-(1/4))^(t-1))
}

###Testando o modelo
coalescent(1,1)

coalescent(2,1)

###Avaliando o resultado
mae<-letters[1:2]
mae
possibilidades<-expand.grid(individuo1=mae,individuo2=mae)

possibilidades$individuo1==possibilidades$individuo2

sum(possibilidades$individuo1==possibilidades$individuo2)/nrow(possibilidades)

###Representação grafica das possibilidades
grafo1<-graph_from_literal(F1-a,F2-a,b)
grafo2<-graph_from_literal(F1-b,F2-a)
grafo3<-graph_from_literal(F1-a,F2-b)
grafo4<-graph_from_literal(F1-b,F2-b,a)

local<-matrix(c(0,0,1,0,0,1,1,1),ncol = 2,nrow=4,dimnames = list(c("F1","F2","a","b"),c("x","y")),byrow=T)

jpeg("01.jpg")
par(mfrow=c(2,2))
local<-local[match(rownames(local),V(grafo1)$name),]
plot(grafo1,layout=local,vertex.size=50,vertex.color="lightblue",
     vertex.label.color="black",vertex.label.cex=1.8,edge.width=4,edge.color="black")

local<-local[match(rownames(local),V(grafo2)$name),]
plot(grafo2,layout=local,vertex.size=50,vertex.color="lightblue",
     vertex.label.color="black",vertex.label.cex=1.8,edge.width=4,edge.color="black")

local<-local[match(rownames(local),V(grafo3)$name),]
plot(grafo3,layout=local,vertex.size=50,vertex.color="lightblue",
     vertex.label.color="black",vertex.label.cex=1.8,edge.width=4,edge.color="black")

local<-local[match(rownames(local),V(grafo4)$name),]
plot(grafo4,layout=local,vertex.size=50,vertex.color="lightblue",
     vertex.label.color="black",vertex.label.cex=1.8,edge.width=4,edge.color="black")
dev.off()

###Mais testes
coalescent(3,1)

mae<-letters[1:3]
mae
possibilidades<-expand.grid(individuo1=mae,individuo2=mae)
possibilidades
possibilidades$individuo1==possibilidades$individuo2
sum(possibilidades$individuo1==possibilidades$individuo2)/nrow(possibilidades)


coalescent(4,1)
mae<-letters[1:4]
possibilidades<-expand.grid(individuo1=mae,individuo2=mae)
possibilidades$individuo1==possibilidades$individuo2
sum(possibilidades$individuo1==possibilidades$individuo2)/nrow(possibilidades)

###Alterando o tempo

coalescent(4,2)

mae<-letters[1:4]
mae

possibilidades<-expand.grid(individuo1=mae,individuo2=mae)
possibilidades

não_coalecer<-sum(!possibilidades$individuo1==possibilidades$individuo2)/nrow(possibilidades)
coalecer<-sum(possibilidades$individuo1==possibilidades$individuo2)/(nrow(possibilidades))

não_coalecer
coalecer

não_coalecer*coalecer

###Olhando ao longo do tempo
jpeg("02.jpg")
plot(coalescent(4,1:20),xlab="Geração para trás",ylab="Probabilidade",bty="n")
dev.off()

jpeg("03.jpg")
plot(cumsum(coalescent(4,1:20)),xlab="Geração para trás",ylab="Somatório das Probabilidades",bty="n")
dev.off()

###Somas ao longo do tempo
sum(coalescent(4,1:10))
sum(coalescent(4,1:20))
sum(coalescent(4,1:40))


