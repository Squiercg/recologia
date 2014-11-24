#Muito do codigo veio desse post
#http://www.sumsar.net/blog/2014/11/how-to-summarize-a-2d-posterior-using-a-highest-density-ellipse/

library(MASS)
library(cluster)
library(plotrix)
library(emdbook)
library(ggplot2)
library(hexbin)
#install.packages("devtools")
#library(devtools)
#install_github("rasmusab/bayesian_first_aid")
library(BayesianFirstAid)

Sigma <- matrix(c(1,0,0,1),2,2)
Sigma

amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)

#
plot(amostras)
t<-seq(0, 2*pi, length = 100)
r<-1
a<-0
b<-0
points(a+r*cos(t),b+r*sin(t),type="l",lwd=5,lty=3,col="red")

#Testando quem está dentro do circulo de raio r
sum(ifelse(amostras[,1]^2 + amostras[,2]^2 <= r^2, TRUE, FALSE))/nrow(amostras)

for(r in seq(0,4,0.2)) {
    cat("Raio ",r," cobre ",(sum(ifelse(amostras[,1]^2 + amostras[,2]^2 <= r^2, TRUE, FALSE))/nrow(amostras))*100,"% dos pontos \n",sep="")
}

#
plot(amostras,frame=F)
i=2
for(r in c(3.1,2.5,1.2)) {
    points(a+r*cos(t),b+r*sin(t),type="l",lwd=5,lty=3,col=i)
    i=i+1
}
legend("topright",title="cobertura",legend=c("99%","95%","50%"),bty="n",lwd=4,lty=3,col=c(2,3,4))

#Distribuição não redondinha
Sigma <- matrix(c(1,0.8,0.8,1),2,2)
Sigma
amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)
#
plot(amostras)

var(amostras[,1])
var(amostras[,2])
cor(amostras)


#
par(mfrow=c(2,2))
Sigma <- matrix(c(1,0.8,0.8,1),2,2)
amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)
plot(amostras)
addtable2plot (x=-2,y=2,table=Sigma,display.colnames=F,bty="o",hlines=T,vlines=T)

Sigma <- matrix(c(1,-0.8,-0.8,1),2,2)
amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)
plot(amostras)
addtable2plot (x=2,y=2,table=Sigma,display.colnames=F,bty="o",hlines=T,vlines=T)

Sigma <- matrix(c(1,0.8,0.8,2),2,2)
amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)
plot(amostras)
addtable2plot (x=-3,y=2,table=Sigma,display.colnames=F,bty="o",hlines=T,vlines=T)

Sigma <- matrix(c(2,0.8,0.8,2),2,2)
amostras<-mvrnorm(n=1000, rep(0, 2), Sigma)
plot(amostras)
addtable2plot (x=3,y=-3,table=Sigma,display.colnames=F,bty="o",hlines=T,vlines=T)



#Desenhando Elipse
t<-seq(0, 2*pi, length = 100)
Xc<-0
Yc<-0
alpha<-pi/3
a<-1
b<-1

#
plot(Xc+a*cos(t)*cos(alpha)-b*sin(t)*sin(alpha),
     Yc+a*cos(t)*sin(alpha)-b*sin(t)*cos(alpha)
     ,type="l",lwd=3,lty=1,col="black")


#
par(mfrow=c(2,2))
alpha<-pi/3.5
plot(Xc+a*cos(t)*cos(alpha)-b*sin(t)*sin(alpha),
     Yc+a*cos(t)*sin(alpha)-b*sin(t)*cos(alpha)
     ,type="l",lwd=3,lty=1,col="black")
legend("topleft",legend=paste("alpha=",round(alpha,2)),bty="n")
alpha<-pi/3
plot(Xc+a*cos(t)*cos(alpha)-b*sin(t)*sin(alpha),
     Yc+a*cos(t)*sin(alpha)-b*sin(t)*cos(alpha)
     ,type="l",lwd=3,lty=1,col="black")
legend("topleft",legend=paste("alpha=",round(alpha,2)),bty="n")
alpha<-pi/2
plot(Xc+a*cos(t)*cos(alpha)-b*sin(t)*sin(alpha),
     Yc+a*cos(t)*sin(alpha)-b*sin(t)*cos(alpha)
     ,type="l",lwd=3,lty=1,col="black")
legend("center",legend=paste("alpha=",round(alpha,2)),bty="n")
alpha<-pi/1.5
plot(Xc+a*cos(t)*cos(alpha)-b*sin(t)*sin(alpha),
     Yc+a*cos(t)*sin(alpha)-b*sin(t)*cos(alpha)
     ,type="l",lwd=3,lty=1,col="black")
legend("topright",legend=paste("alpha=",round(alpha,2)),bty="n")


#Ajuste de elipse
ajuste <- cov.mve(amostras, quantile.used = nrow(amostras) * 0.50)
pontos_na_ellipse <- amostras[ajuste$best, ]
limite_ellipse <- predict(ellipsoidhull(pontos_na_ellipse))
plot(amostras)
points(limite_ellipse[,1],limite_ellipse[,2],type="l",lwd=5,lty=3,col="red")
legend("topleft", "50%",lwd=5,lty=3,col="red")



#Varios ajustes
plot(amostras)
i<-2
for(cobertura in c(0.99, 0.95, 0.5)) {
    ajuste <- cov.mve(amostras, quantile.used = nrow(amostras) * cobertura)
    pontos_na_ellipse <- amostras[ajuste$best, ]
    limite_ellipse <- predict(ellipsoidhull(pontos_na_ellipse))
    points(limite_ellipse[,1],limite_ellipse[,2],type="l",lwd=5,lty=3,col=i)
    i<-i+1
}
legend("topleft",legend=c("99%","95%","50%"),lwd=5,lty=3,col=2:4)


#Plot com poligonos
plot(amostras,type="n")
i<-2
for(cobertura in c(0.99, 0.95, 0.5)) {
    ajuste <- cov.mve(amostras, quantile.used = nrow(amostras) * cobertura)
    pontos_na_ellipse <- amostras[ajuste$best, ]
    limite_ellipse <- predict(ellipsoidhull(pontos_na_ellipse))
    polygon(limite_ellipse, col = i, border = NA)
    i<-i+1
}
points(amostras[,1],amostras[,2],pch=19)
legend("topleft",legend=c("99%","95%","50%"),pch=1,col=2:4)

#Distribuição posterior
plot(amostras)
HPDregionplot(amostras, prob = c(0.95, 0.75, 0.5), col=2:4, lwd=3,lty=3, add=TRUE)
legend("topleft", legend = c("95%", "75%", "50%"), col = 2:4, lty=3, lwd=3)



#ggplot2 hexagonos
qplot(amostras[,1], amostras[,2], geom=c("hex"))

#Bayes plot
amostras<-mvrnorm(n=50, c(0,0), Sigma)
ajuste <- bayes.cor.test(amostras[,1], amostras[,2])
plot(ajuste)

