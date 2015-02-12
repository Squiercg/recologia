#post:
#http://recologia.com.br/2015/02/diferenca-entre-correlacao-e-regressao

#Gerando um exemplo
set.seed(123)
n<-30
x<-runif(n,1,10)
y<-rnorm(n,5-x)

#figura 1
plot(y~x,frame=F)


#figura 2
plot(y~x,pch=19,frame=F)
modelo<-lm(y~x)
abline(modelo)
for(i in 1:n) { lines(cbind(x,x)[i,],cbind(predict(modelo),y)[i,]) }


#Figuras 3.1 e 3.2
inclinacoes <- seq(-10,10,0.1)
interceptos <- seq(3,7,0.1)

superficie<-matrix(0,nrow=length(interceptos),ncol=length(inclinacoes))
for(i in 1:length(interceptos)) {
    for(j in 1:length(inclinacoes)) {
        superficie[i,j]<-sum((y - interceptos[i] - inclinacoes[j]*x)^2)
    }
}
#3.1
persp(interceptos,inclinacoes,superficie,theta = 20, phi = 20)

SSE <- function(i) sum((y - 5 - inclinacoes[i]*x)^2)
#3.2
par(mfrow=c(2,2))
for(i in 3:6) {
plot(inclinacoes,sapply(1:length(inclinacoes),SSE),type="l",
     main=paste("Intercepto = ",i),xlab="inclinação b",
     ylab="soma dos resíduos quadrados",lty=3,cex=3)
}


#Famosos 5
sum(x);sum(x^2);sum(y);sum(y^2);sum(x*y)
XY <- cbind(1,x,y)
t(XY) %*% XY

SSX<-sum(x^2)-sum(x)^2/n

SSY<-sum(y^2)-sum(y)^2/n

SSXY <- sum(x*y)-(sum(x)*sum(y))/n

#Definindo inclinação e intercepto
inclinacao<-SSXY/SSX
inclinacao

intercepto<-mean(y)-inclinacao*mean(x)
intercepto

#Inclinação a partir da correlação
inclinacao
cor(y,x)*(sd(y)/sd(x))

#R^2
summary(lm(y~x))
round(cor(y,x)^2,digits=4)


#Comparando valores encontrados
lm(y~x)
round(intercepto,digits=3)
round(inclinacao,digits=3)

#figura 4
plot(y~x,frame=F,type="n")
abline(v=mean(x),lty=3,cex=2)
abline(h=mean(y),lty=3,cex=2)

indices<-x>mean(x) & y>mean(y)
points(x[indices],y[indices],col=1,pch=19)
indices<-x<=mean(x) & y>mean(y)
points(x[indices],y[indices],col=2,pch=19)
indices<-x<=mean(x) & y<=mean(y)
points(x[indices],y[indices],col=3,pch=19)
indices<-x>mean(x) & y<=mean(y)
points(x[indices],y[indices],col=4,pch=19)
dev.off()

#modelo sem correlação
x<-runif(n,1,10)
y<-rnorm(n,mean(x))

#figura 5
plot(y~x,frame=F,type="n")
abline(v=mean(x),lty=3,cex=2)
abline(h=mean(y),lty=3,cex=2)

indices<-x>mean(x) & y>mean(y)
points(x[indices],y[indices],col=1,pch=19)
indices<-x<=mean(x) & y>mean(y)
points(x[indices],y[indices],col=2,pch=19)
indices<-x<=mean(x) & y<=mean(y)
points(x[indices],y[indices],col=3,pch=19)
indices<-x>mean(x) & y<=mean(y)
points(x[indices],y[indices],col=4,pch=19)
dev.off()

#Calculando a correlação
cor(y,x)
1/(n-1)*sum(((y-mean(y))/sd(y))*((x-mean(x))/sd(x)))
