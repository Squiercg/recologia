##http://recologia.com.br/2016/07/distancia-ate-o-vizinho-mais-proximo/
rm(list=ls())
##
set.seed(2)

##Distribuição de pontos aleatórios
x <- runif(100)
y <- runif(100)

##Num mapa
jpeg("01.jpg")
plot(x,y,pch=19,col="black",main="Mapa")
dev.off()

##Distância entre pontos
distancia <- function(x1,y1,x2,y2){
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

##Menor Distância para cada ponto

##Vetor para guardar a menor distância
r <- vector()
##Vetor para guardar o indice do ponto mais próximo
nn <- vector()
#Vetor auxiliar para guardar todas as distâncias
d <- vector()

##Para cada ponto i
for (i in 1:100) {
    ##Calcular a distancia dele até o ponto j
    for (j in 1:100){
        d[j] <- distancia(x[i],y[i],x[j],y[j])
    }
    ##Qual a menor distância? Note que tiramos o i, porque a
    ##distância de um ponto pra ele mesmo não interessa, é 0
    ##Anotamos a distância
    r[i] <- min(d[-i])
    ##E o indice do ponto correspondente
    nn[i] <- which(d==r[i])
}

##Desenhando uma linha até o ponto mais próximo
jpeg("02.jpg")
plot(x,y,pch=19,col="black")
for (i in 1:100){
    lines(c(x[i],x[nn[i]]),c(y[i],y[nn[i]]),col="red",lty=3,lwd=2)
}
dev.off()

##Descobrindo quem está mais próximo da borda do que do vizinho mais próximo:

##bordas
dist_borda_superior <- 1-y
dist_borda_direita <- 1-x

##Distância para a borda
borda <- pmin(x,y,dist_borda_superior,dist_borda_direita)

##Quantos pontos estão mais próximos da borda
##Do que do vizinho mais próximo
sum(borda<r)

##
jpeg("03.jpg")
plot(x,y,pch=19)
indice <- which(borda<r)
points(x[indice],y[indice],col="red",cex=1.5,lwd=2)
dev.off()

jpeg("04.jpg")
layout(matrix(1:4,ncol=2,nrow=2))
hist(borda,xlim=c(0,0.5),ylim=c(0,20),breaks=seq(0,0.5,length.out = 20),main="Distância para borda")
hist(borda[-indice],xlim=c(0,0.5),ylim=c(0,20),breaks=seq(0,0.5,length.out = 20),main="Sem efeito de borda")
hist(r,xlim=c(0,0.15),ylim=c(0,20),breaks=seq(0,0.15,length.out = 20),main=paste("Vizinho Mais Próximo\n Média:",round(mean(r),4)))
hist(r[-indice],xlim=c(0,0.15),ylim=c(0,20),breaks=seq(0,0.15,length.out = 20),
     main=paste("Sem efeito de borda\n Média:",round(mean(r[-indice]),4)))
dev.off()
