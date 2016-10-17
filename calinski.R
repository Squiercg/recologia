##http://recologia.com.br/2016/09/escolhendo-um-valor-de-k-para-o-kmeans-usando-o-criterio-de-calinski/
##Pacotes utilizados
library(vegan)

##Gerando um exemplo
x<-c(1,1,2,2)
y<-c(1,2,1,2)

set.seed(123)
exemplo<-data.frame(
    x=c(rnorm(30,x[1],0.3),rnorm(30,x[2],0.2),rnorm(30,x[3],0.3),rnorm(30,x[4],0.2)),
    y=c(rnorm(30,y[1],0.2),rnorm(30,y[2],0.2),rnorm(30,y[3],0.2),rnorm(30,y[4],0.2))
)
grupo<-factor(rep(letters[1:4],each=30))

##Exemplo gerado
plot(exemplo$x,exemplo$y,col=as.numeric(grupo),pch=19,frame=F,xlab="eixo x",ylab="eixo y")
legend("topright",legend=c(levels(grupo)),col=unique(as.numeric(grupo)),pch=19,bty="n")

##Dados não classificados
plot(exemplo$x,exemplo$y,pch=19,frame=F,xlab="eixo x",ylab="eixo y")

ajuste<-kmeans(exemplo,4)

##Resultado vc original
par(mfrow=c(2,1))
plot(exemplo$x,exemplo$y,col=as.numeric(grupo),pch=19,xlab="eixo x",ylab="eixo y")
plot(exemplo$x,exemplo$y,col=ajuste$cluster,pch=19,xlab="eixo x",ylab="eixo y")


##Comparando varios valores de k numa única figura
par(mfrow=c(2,2),mar=c(4,4,2,2))
ajuste<-kmeans(exemplo,2)
plot(exemplo$x,exemplo$y,col=ajuste$cluster,pch=19,xlab="eixo x",ylab="eixo y",frame=F,main="K=2")
ajuste<-kmeans(exemplo,3)
plot(exemplo$x,exemplo$y,col=ajuste$cluster,pch=19,xlab="eixo x",ylab="eixo y",frame=F,main="K=3")
ajuste<-kmeans(exemplo,6)
plot(exemplo$x,exemplo$y,col=ajuste$cluster,pch=19,xlab="eixo x",ylab="eixo y",frame=F,main="K=6")
ajuste<-kmeans(exemplo,8)
plot(exemplo$x,exemplo$y,col=ajuste$cluster,pch=19,xlab="eixo x",ylab="eixo y",frame=F,main="K=8")


##
ajuste<-cascadeKM(exemplo,2,10,criterion = 'calinski')

##Plot original
plot(ajuste)

##Fazendo nosso próprio plot
plot(2:10,ajuste$results[2,],type="b",pch=19,xlab="Número de Grupos",ylab="Valor de Calinski",frame=F)
abline(v=which.max(ajuste$results[2,])+1,lty=3,lwd=3,col="red")
