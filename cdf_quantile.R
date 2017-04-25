

##jpeg("01.jpg")
curve(dexp(x,1/5),0,20,frame=F,xlab="Tempo em anos",ylab="Probabilidade")
polygon(c(seq(6,20,0.1),seq(20,6,-0.1)),c(dexp(seq(6,20,0.1),1/5),rep(0,141)),col="red")
text(8,0.02,"S(x)")
text(4,0.02,"F(x)")
##dev.off()


pexp(6,1/5,lower.tail = F)

1-pexp(6,1/5,lower.tail = F)



##jpeg("02.jpg")
curve(dexp(x,1/5),0,20,frame=F,xlab="Tempo em anos",ylab="Probabilidade")
polygon(c(seq(6,20,0.1),seq(20,6,-0.1)),c(dexp(seq(6,20,0.1),1/5),rep(0,141)),col="red")
text(8,0.02,paste("S(x)=",round(pexp(6,1/5,lower.tail = F),3),sep=""))
text(4,0.02,paste("F(x)=",1-round(pexp(6,1/5,lower.tail = F),3),sep=""))
##dev.off()


qexp(0.5,1/5)

##jpeg("03.jpg")
q50<-qexp(0.5,1/5)
curve(dexp(x,1/5),0,20,frame=F,xlab="Tempo em anos",ylab="Probabilidade")
polygon(c(seq(q50,20,0.1),seq(20,q50,-0.1)),c(dexp(seq(q50,20,0.1),1/5),rep(0,166)),col="red")
text(q50-2,0.02,paste("S(x)=",round(pexp(q50,1/5,lower.tail = F),3),sep=""))
text(q50+2,0.02,paste("F(x)=",1-round(pexp(q50,1/5,lower.tail = F),3),sep=""))
mtext(round(q50,2),side=1,at=q50)
abline(v=q50,col="blue",lty=3,lwd=2)
##dev.off()
