##Explicação do código em http://recologia.com.br/2017/11/generalized-additive-model-quando-nao-sabemos-que-modelo-usar/

###Simulando Dados
set.seed(123)
preditor <- runif(30,0,4*pi)
resposta_pot<-5+2*(preditor)^2+rnorm(30,0,10)
resposta_seno<-5+2*sin(preditor)+rnorm(30)

##
jpeg("01.jpg")
plot(resposta_pot~preditor,frame=F,pch=19,xlab="Preditor", ylab="Resposta")
dev.off()

##
jpeg("02.jpg")
plot(resposta_pot~preditor,frame=F,pch=19,xlab="Preditor", ylab="Resposta")
modelo_linear <- lm(resposta_pot~preditor)
abline(modelo_linear, col="red")
dev.off()

##
summary(modelo_linear)

##
jpeg("03.jpg")
plot(resposta_pot~preditor,frame=F,pch=19,xlab="Preditor", ylab="Resposta")
curve(5+2*x^2,add=T,lwd=2,col="blue")
legend("topleft",col=c("blue","red"),lwd=c(2,1),legend=c("Realidade","Modelo"),bty="n")

modelo_linear <- lm(resposta_pot~preditor)
abline(modelo_linear, col="red")
novos_dados <- seq(0,4*pi,0.25)
pred_interval <- predict(modelo_linear, newdata=data.frame(preditor=novos_dados), interval="prediction",level = 0.95)
lines(novos_dados, pred_interval[,2], col="orange", lty=2)
lines(novos_dados, pred_interval[,3], col="orange", lty=2)
dev.off()

##
jpeg("04.jpg")
par(mfrow=c(2,2))
plot(modelo_linear)
dev.off()

##
jpeg("05.jpg")
plot(resposta_seno~preditor,frame=F,pch=19,ylim=c(0,12),xlab="Preditor",ylab="Resposta")
curve(5+2*sin(x),add=T,lwd=2,col="blue")
legend("topleft",col=c("blue","red"),lwd=c(2,1),legend=c("Realidade","Modelo"),bty="n")

modelo_linear <- lm(resposta_seno~preditor)
abline(modelo_linear, col="red")
novos_dados <- seq(0,4*pi,0.25)
pred_interval <- predict(modelo_linear, newdata=data.frame(preditor=novos_dados), interval="prediction",level = 0.95)
lines(novos_dados, pred_interval[,2], col="orange", lty=2)
lines(novos_dados, pred_interval[,3], col="orange", lty=2)
dev.off()

##
summary(modelo_linear)

##
jpeg("06.jpg")
par(mfrow=c(2,2))
plot(modelo_linear)
dev.off()

##Pacote utilizado  para o GAM
##install.packages("mgcv")
library(mgcv)

## Ajuste de modelo
modelo_gam<- gam(resposta_pot~s(preditor,k=5,fx=T),family = gaussian)
summary(modelo_gam)

## Visualização do modelo
jpeg("07.jpg")
plot(modelo_gam)
dev.off()

##
modelo_gam<- gam(resposta_seno~s(preditor,k=5,fx=T),family = gaussian)
summary(modelo_gam)

##
jpeg("08.jpg")
plot(modelo_gam)
dev.off()

##
jpeg("09.jpg")
plot(resposta_seno~preditor,frame=F,pch=19,ylim=c(0,12))

points(x=c(4,4,6,6,4),y=c(2,4,4,2,2),type="l",lty=2)
selecao <- preditor>=4 & preditor<=6
modelo_linear <- lm(resposta_seno[selecao]~preditor[selecao])
curve(coef(modelo_linear)[1]+x*coef(modelo_linear)[2],4,6,add=T,lwd=2,col="red")
dev.off()

##
jpeg("10.jpg")
plot(resposta_seno~preditor,frame=F,pch=19,ylim=c(0,12))

points(x=c(4,4,6,6,4)+3,y=c(2,4,4,2,2)*2+1,type="l",lty=2)
selecao <- preditor>=7 & preditor<=9
modelo_linear <- lm(resposta_seno[selecao]~preditor[selecao])
curve(coef(modelo_linear)[1]+x*coef(modelo_linear)[2],7,9,add=T,lwd=2,col="red")
dev.off()

##
jpeg("11.jpg")
plot(resposta_seno~preditor,frame=F,pch=19,ylim=c(0,12))

points(x=c(4,4,6,6,4),y=c(2,4,4,2,2),type="l",lty=2)
selecao <- preditor>=4 & preditor<=6
modelo_linear <- lm(resposta_seno[selecao]~preditor[selecao])
curve(coef(modelo_linear)[1]+x*coef(modelo_linear)[2],4,6,add=T,lwd=2,col="red")

points(x=c(4,4,6,6,4)+3,y=c(2,4,4,2,2)*2+1,type="l",lty=2)
selecao <- preditor>=7 & preditor<=9
modelo_linear <- lm(resposta_seno[selecao]~preditor[selecao])
curve(coef(modelo_linear)[1]+x*coef(modelo_linear)[2],7,9,add=T,lwd=2,col="red")
dev.off()

##Testando gam contra dados aleatórios
resposta_aleatoria <- rnorm(30)

##
jpeg("12.jpg")
plot(resposta_aleatoria~preditor,pch=19,frame=F)
dev.off()

modelo_gam<- gam(resposta_aleatoria~s(preditor,k=5,fx=T),family = gaussian)
summary(modelo_gam)

##
jpeg("13.jpg")
plot(modelo_gam)
dev.off()

##Definindo um valor de K
modelo_gam<- gam(resposta_seno~s(preditor,k=2,fx=T),family = gaussian)
jpeg("14.jpg")
hist(resid(modelo_gam))
dev.off()

##
modelo_gam<- gam(resposta_seno~s(preditor,k=5,fx=T),family = gaussian)
jpeg("15.jpg")
hist(resid(modelo_gam))
dev.off()

##Auto ajuste de k
modelo_gam<- gam(resposta_seno~s(preditor,k=-1,fx=T),family = gaussian)
jpeg("16.jpg")
hist(resid(modelo_gam))
dev.off()
