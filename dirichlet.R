##http://recologia.com.br/2018/05/distribuicao-de-dirichlet/
library(Compositional)
library(MCMCpack)

##
parametros <- c(1,1,1)
amostra <- rdirichlet(10,parametros)
amostra

rowSums(amostra)

ddirichlet(amostra[1,], c(1,2,3))

##Explorando a distribuição
amostra <- rdirichlet(200, c(.1,.1,.1) )
bivt.contour(amostra)

amostra <- rdirichlet(200, c(1,1,1) )
bivt.contour(amostra)

amostra <- rdirichlet(200, c(10,10,10) )
bivt.contour(amostra)

amostra <- rdirichlet(200, c(100,100,100) )
bivt.contour(amostra)

amostra <- rdirichlet(200, c(1000,1000,1000) )
bivt.contour(amostra)

##Lembrando dos dados iris
head(iris)

##PCA dos dados iris
coordenadas <- princomp(iris[,1:4])$scores[,1:2]
plot(coordenadas[,1],coordenadas[,2],pch=19,col=as.numeric(iris$Species),frame=F,xlab="PCA 1",ylab="PCA 2")


##Plot em três dimensões
setosa <- iris[iris$Species=="setosa",c(3,4,1)]
setosa <- as.matrix(setosa/rowSums(setosa))
bivt.contour(setosa)

##
versicolor <- iris[iris$Species=="versicolor",c(3,4,1)]
versicolor <- as.matrix(versicolor/rowSums(versicolor))
virginica <- iris[iris$Species=="virginica",c(3,4,1)]
virginica <- as.matrix(virginica/rowSums(virginica))


##Plot das 3 espécies
par(mfrow=c(1,3))
bivt.contour(setosa)
title("setosa")

bivt.contour(versicolor)
title("versicolor")

bivt.contour(virginica)
title("virginica")
