#Gerando dados
set.seed(21)
m_menten <- function(C,Vmax,Km) { return( (Vmax * C)/( Km + C) ) }
con<-runif(30,0,4)
v<-m_menten(con,Vmax=3.4,Km=0.4)
v<-v+rnorm(30,0,0.08)
dados<-data.frame(Con=con,Vel=v)

#Figura 1, Caracteristicas do modelo
plot(Vel~Con,frame=F,ylim=c(0,4),xlim=c(0,4.5),ylab="Velocidade",xlab="Concentração",pch=19,data=dados)
abline(h=3.4,lty=2,col="red")
text(4,3.5,"Vmax")
abline(h=0.5*(3.4),lty=2,col="red")
text(4,(0.5*(3.4))-0.1,expression("1/2 Vmax"))
lines(x=c(0.4,0.4),y=c(0,0.5*(3.4)),lty=2,col="red")
text(0.55,1,"Km")

#Ajustando um modelo não linear para os dados
nls.menten<-nls(Vel~(Vmax * Con)/(Km + Con),data=dados,start=list(Vmax=1,Km=1),algorithm="default")
summary(nls.menten)

#Figura 2 - ajuste modelo não linear
plot(Vel~Con,frame=F,ylim=c(0,4),xlim=c(0,6),ylab="Velocidade",xlab="Concentração",pch=19,data=dados)
points(seq(0,6,0.01),predict(nls.menten,newdata=data.frame(Con=seq(0,6,0.01))),type="l",lty=2,col="blue")
legend("topleft",lty=2,col="blue",legend="Ajuste com nls()",bty="n")

#Figura 3.
plot(c(1/Vel)~c(1/Con),frame=F,ylim=c(0,1),xlim=c(0,6),ylab="1/Velocidade",xlab="1/Concentração",pch=19,data=dados)

#Ajustando um modelo Geral Linear
glm.menten<-glm(Vel ~c(Con^-1), data = dados,family = gaussian(link = "inverse"))
summary(glm.menten)

#Retornando os valores de Vmax e Km
coef(glm.menten)[1]^-1
coef(glm.menten)[2]*(coef(glm.menten)[1]^-1)

#Figura 4 - Ajuste com modelo linear
plot(c(1/Vel)~c(1/Con),frame=F,ylim=c(0,1),xlim=c(0,6),ylab="1/Velocidade",xlab="1/Concentração",pch=19,data=dados)
points(c(seq(0.1,6,0.01)),predict(glm.menten,newdata=data.frame(Con=c(1/seq(0.1,6,0.01)))),type="l",lty=2,col="green")
legend("topleft",lty=2,col="green",legend="Ajuste com glm()",bty="n")


#Figura 5 - Comparando os dois ajustes
plot(Vel~Con,frame=F,ylim=c(0,4),xlim=c(0,6),ylab="Velocidade",xlab="Concentração",pch=19,data=dados)
points(seq(0,6,0.01),predict(nls.menten,newdata=data.frame(Con=seq(0,6,0.01))),type="l",lty=2,col="blue")
points(c(seq(0.1,6,0.01))^-1,predict(glm.menten,newdata=data.frame(Con=c(1/seq(0.1,6,0.01))))^-1,type="l",lty=2,col="green")
legend("topleft",lty=2,col=c("blue","green"),legend=c("Ajuste com nls","Ajuste com glm"),bty="n")
