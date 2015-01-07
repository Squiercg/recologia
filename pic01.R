#http://www.phytools.org/eqg/Exercise_4.1/
set.seed(123)

#Tempo
t <- 0:100
#Variação
sig2 <- 0.01
## Primeiro simulamos um conjunto de desvios aléatorios
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## Depois nos computamos a soma comulativa
x <- c(0, cumsum(x))

#Figura1
jpeg("01.jpg")
plot(t, x, type = "l", ylim = c(-2, 2),frame=F,xlab="Tempo (Gerações)",ylab="Variação fenotípica",lty=3)
points(0,0,pch=19,col="red")
dev.off()

#Agora vamos simular varias espécies
nsim <- 100
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t)-1)
X <- cbind(rep(0, nsim), t(apply(X, 1, cumsum)))

#Figura 2
jpeg("02.jpg")
plot(t, X[1, ], xlab = "Tempo", ylab = "Variação fenotípica", ylim = c(-2.5, 2.5), type = "l",frame=F,lty=3)
apply(X[2:nsim, ], 1, function(x, t) lines(t, x,lty=3), t = t)
points(0,0,pch=19,col="red")
dev.off()

#Figura 3
jpeg("03.jpg")
hist(X[,101],xlim=c(-3,3),breaks =seq(-3,3,0.2),main="Após 100 gerações",freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
dev.off()

#Figura 4
jpeg("04.jpg")
par(mfrow=c(2,2))
hist(X[,11],xlim=c(-3,3),breaks =seq(-3,3,0.2),main=paste("Gerações = 10, Desvio Padrão = ",round(sd(X[,11]),2)),freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
hist(X[,26],xlim=c(-3,3),breaks =seq(-3,3,0.2),main=paste("Gerações = 25, Desvio Padrão = ",round(sd(X[,26]),2)),freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
hist(X[,51],xlim=c(-3,3),breaks =seq(-3,3,0.2),main=paste("Gerações = 51, Desvio Padrão = ",round(sd(X[,51]),2)),freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
hist(X[,101],xlim=c(-3,3),breaks =seq(-3,3,0.2),main=paste("Gerações = 101, Desvio Padrão = ",round(sd(X[,101]),2)),freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
dev.off()

#Simulação usando for
X <- matrix(0, nsim, length(t))
for (i in 1:nsim) {
    X[i, ] <- c(0, cumsum(rnorm(n = length(t) - 1, sd = sqrt(sig2))))
}

#Figura 5
jpeg("05.jpg")
par(mfrow=c(1,1))
plot(t, X[1, ], xlab = "Tempo", ylab = "Variação fenotípica", ylim = c(-2.5, 2.5), type = "l",frame=F,lty=3)
for (i in 1:nsim) {
    lines(t, X[i, ],lty=3)
}
points(0,0,pch=19,col="red")
dev.off()

#Menor Variação
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2/10)), nsim, length(t) -   1)
X <- cbind(rep(0, nsim), t(apply(X, 1, cumsum)))

#Figura 6
jpeg("06.jpg")
plot(t, X[1, ], xlab = "Tempo (Gerações)", ylab = "Variação fenotípica", ylim = c(-2, 2), type = "l",lty=3,frame=F)
apply(X[2:nsim, ], 1, function(x, t) lines(t, x,lty=3), t = t)
points(0,0,pch=19,col="red")
dev.off()

#Variação vs Geração
nsim <- 10000
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t)-1)
X <- cbind(rep(0, nsim), t(apply(X, 1, cumsum)))
v <- apply(X, 2, var)

#Figura 7
jpeg("07.jpg")
plot(t, v, type = "l", xlab = "Tempo (Gerações)", ylab = "Variancia entre gerações",frame=F)
dev.off()

#
library(phytools)
t <- 100  # Tempo Total
n <- 30  # Número de espécies
b <- (log(n) - log(2))/t
tree <- pbtree(b = b, n = n, t = t, type = "discrete")
tree$tip.label <- paste("Sp",sub("t","",tree$tip.label))

#Figura 8
jpeg("08.jpg")
plot(tree)
dev.off()


## Simulando evolução ao longo de cada vertice (espécie ancestral)
X <- lapply(tree$edge.length, function(x) c(0, cumsum(rnorm(n = x, sd = sqrt(sig2)))))
## Reordenando os vertices da arvore em pre-ordem transversal
cw <- reorder(tree)
## Agora simulando na arvore
ll <- tree$edge.length + 1
for (i in 1:nrow(cw$edge)) {
    pp <- which(cw$edge[, 2] == cw$edge[i, 1])
    if (length(pp) > 0) {
        X[[i]] <- X[[i]] + X[[pp]][ll[pp]]
    } else {
        X[[i]] <- X[[i]] + X[[1]][1]
    }
}
## Pegando o ponto de inicio e fim de cada vertice para a figura
H <- nodeHeights(tree)


#Figura 9
jpeg("09.jpg")
#Linhas da simulação
plot(H[1, 1], X[[1]][1], ylim = c(-2.5,2.5), xlim=c(0,120), xlab = "Tempo (Gerações)", ylab = "Fenótipo",col="red",pch=19,frame=F)
for (i in 1:length(X)) {
    lines(H[i, 1]:H[i, 2], X[[i]],lty=3)
}
#Pontos internos da arvore onde começa o movimento browniano
for (i in 1:nrow(H)) {
    if(H[i, 2]!=100){
        points(H[i, 2], X[[i]][length(X[[i]])],pch=19,col="red")
    }
}
## Adicionando os nomes das espécies
yy <- sapply(1:length(tree$tip.label), function(x, y) which(x == y), y = tree$edge[,2])
yy <- sapply(yy, function(x, y) y[[x]][length(y[[x]])], y = X)
text(x = max(H)+5, y = yy, tree$tip.label,cex=0.6)
dev.off()

#Separando os valores para o histograma dess simulação
saida<-rep(NA,n)
contador<-1
for (i in 1:nrow(H)) {
    if(H[i, 2]==100){
         saida[contador] <- X[[i]][length(X[[i]])]
         contador <- contador + 1
    }
}

#Figura 10
jpeg("10.jpg")
hist(saida,xlim=c(-3,3),breaks =seq(-3,3,0.2),main="Após 100 gerações",freq =F
    ,ylab="Densidade",xlab="Classes de variação fenotípica")
dev.off()

## Simulando a evolução Browniana em uma arvores usando o fastBM
x <- fastBM(tree, sig2 = sig2, internal = TRUE)

#Figura 11
jpeg("11.jpg")
phenogram(tree, x, spread.labels = TRUE, spread.cost = c(1, 0))
dev.off()
