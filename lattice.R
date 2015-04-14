library(lattice)
set.seed(1)

##################################
## Gerando os dados
##################################
n.grupos <- 4 # Número de populações
n.amostras <- 30 # Número de Cobras por amostras
n <- n.grupos * n.amostras # Total de amostras (Total de passarinhos)
pop <- gl(n = n.grupos, k = n.amostras) # Indicador da população
# Tamanho do corpo (cm)
tamanho.original <- runif(n, 45, 70)
mn <- mean(tamanho.original)
sd <- sd(tamanho.original)
#Mesmo que usar o comando scale(), isso faz a média ser zero
tamanho <- (tamanho.original - mn) / sd
Xmat <- model.matrix(~pop*tamanho-1-tamanho) #Aqui estamos fazendo o
#contrario que o comando formula faz, para simular uai
intercepito.mean <- 230	 # intercepto médio
intercepito.sd <- 20 # desvio dos interceptos
inclinação.mean <- 60 # inclinação média
inclinação.sd <- 30 # desvio das inclinações
intercepitos<-rnorm(n = n.grupos, mean = intercepito.mean, sd = intercepito.sd)
inclinações <- rnorm(n = n.grupos, mean = inclinação.mean, sd = inclinação.sd)
todos.os.efeitos <- c(intercepitos,inclinações) # Juntando tudo
lin.pred <- Xmat[,] %*% todos.os.efeitos # Preditores lineares
eps <- rnorm(n = n, mean = 0, sd = 30) # residuos
# resposta = preditor linear + residuo
massa <- lin.pred + eps
dados<-data.frame(pop,tamanho,massa)
#deixando somente os dados em dataframe, para acessar certo
rm(list=ls()[-1])

##################################
##Figuras tipo lattice
##################################
head(dados)

#figura 1
plot(massa~tamanho,data=dados)

#figura 2
plot(massa~tamanho,pch=19,frame=F,xlab="Tamanho",ylab="Massa",col=dados$pop,data=dados)
legend("topleft",pch=19,col=1:4,legend=paste("População",1:4),bty="n")

#figura 3
layout(matrix(1:4,ncol=2,nrow=2,byrow=T))
for(i in 1:4) {
    plot(massa~tamanho,pch=19,frame=F,xlab="Tamanho",ylab="Massa",
         col=i,data=dados[dados$pop==i,],main=paste("População",i),
         xlim=c(min(dados$tamanho),max(dados$tamanho)),
         ylim=c(min(dados$massa),max(dados$massa)) )
    ordem <- order(dados[dados$pop==i,"tamanho"])
    ajuste<-loess(massa~tamanho,dados[dados$pop==i,])
    points(ajuste$fitted[ordem]~dados[dados$pop==i,"tamanho"][ordem],
           type="l",col="red",lty=3,lwd=2)
}

#figura 4
layout(matrix(c(1:3,rep(4,3)),ncol=3,nrow=2,byrow=T))
for(i in 1:4) {
    plot(massa~tamanho,pch=19,frame=F,xlab="Tamanho",ylab="Massa",
         col=i,data=dados[dados$pop==i,],main=paste("População",i),
         xlim=c(min(dados$tamanho),max(dados$tamanho)),
         ylim=c(min(dados$massa),max(dados$massa)) )
    ordem <- order(dados[dados$pop==i,"tamanho"])
    ajuste<-loess(massa~tamanho,dados[dados$pop==i,])
    points(ajuste$fitted[ordem]~dados[dados$pop==i,"tamanho"][ordem],
           type="l",col="red",lty=3,lwd=2)
}

#figura 5
xyplot(massa~tamanho|pop,data=dados)

#figura 6
xyplot(massa~tamanho|pop,data=dados,pch=19,col=dados$pop)

#figura 7
meu.panel = function(x,y,subscripts) {
    panel.xyplot(x,y,col=dados$pop[subscripts],pch=19)
}

xyplot(massa~tamanho|pop,data=dados,panel=meu.panel)

                                        #figura 8
meu.panel = function(x,y,subscripts) {
    panel.xyplot(x,y,col=dados$pop[subscripts],pch=19)
    panel.loess(x, y,col="red",lty=2,lwd=2)
}

trellis.par.set(strip.background=list(col="white"),box.rectangle=list(col="black"),add.text=list(font=3),
box.umbrella=list(col="black"),box.dot=list(col="black"),plot.symbol=list(col="black",pch=1,cex=1))

xyplot(massa~tamanho|pop,data=dados,panel=meu.panel,xlab="Tamanho",ylab="Massa",
       strip = strip.custom(factor.levels =paste("População",1:4)))
