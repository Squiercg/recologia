#Tempo para encontrar uma presa.
tep<-function(ap) {
    return (1/ap)
}

sequencia<-seq(0.2,1,0.01)

jpeg("01.jpg")
plot(sequencia,tep(sequencia),pch=19,type="b",frame=F,xaxt="n",
     ylab="Tempo para encontrar uma presa",xlab="Abundância da presa no ambiente")
axis(1,at=c(0.2,0.4,0.6,0.8,1),labels=F)
mtext("Muito raro", side = 1, line = 1, at = 0.2)
mtext("Raro", side = 1, line = 1, at = 0.4)
mtext("Mais ou menos", side = 1, line = 1, at = 0.6)
mtext("Abundante", side = 1, line = 1, at = 0.8)
mtext("Muito abundante", side = 1, line = 1, at = 1)
dev.off()

tepm<-function(ap,tm) {
    return ((1/ap)+tm)
}

jpeg("02.jpg")
curve(tep(x),0.1,1,frame=F,ylab="Tempo para encontrar uma presa",xlab="Abundância da presa no ambiente",xaxt="n")
axis(1,at=c(0.1,0.4,0.6,0.8,1),labels=F)
mtext("Muito raro", side = 1, line = 1, at = 0.1)
mtext("Raro", side = 1, line = 1, at = 0.4)
mtext("Mais ou menos", side = 1, line = 1, at = 0.6)
mtext("Abundante", side = 1, line = 1, at = 0.8)
mtext("Muito abundante", side = 1, line = 1, at = 1)
tm<-seq(1,5,1)
for(i in tm){
    curve(tepm(x,i),col=i,add=T,lwd=2,lty=2)
}
legend("topright",col=c(1,tm),lwd=c(1,rep(2,5)),lty=c(1,rep(2,5)),legend=paste("Tempo =",c(0,tm)),bty="n")
dev.off()


#
tiw<-function(aw,hw) {
    return(1/aw + hw)
    }

#Larva
hw<-1
aw<-seq(0.000,0.03,0.0005)

#Besouro
ab<-0.5*aw
hb<-60

jpeg("03.jpg")
plot(aw,tiw(aw,hw),pch=19,cex=0.5,type="b",col=2,frame=F,xlab="Abundância",ylab="Tempo total manipulação",
     ylim=c(0,200),xlim=c(0,0.03))
points(aw,tiw(ab,hb),pch=19,cex=0.5,type="b",col=3)
points(aw,tiwb(aw,ab,hw,hb),type="l",col=1,lwd=1,lty=3)

legend("topright",col=c(2,3,1),cex=1,pch=19,lty=c(1,1,3),bty="n",legend=c("Larva","Besouro","Ambos"))
dev.off()


tiwb<- function(aw,ab,hw,hb) {
    return(1/(aw+ab)+hw*aw/(aw+ab)+hb*ab/(aw+ab))
}

jpeg("04.jpg")
#
plot(aw,tiwb(aw,ab,hw,hb),type="l",col=1,lwd=2,lty=2,frame=F,xlab="Abundância",ylab="Tempo total manipulação",
     ylim=c(0,200),xlim=c(0,0.03),main="Tempo de manipulação:\n Larvas = 1\n Besouros=60")

#
ab<-aw;
points(aw,tiwb(aw,ab,hw,hb),type="l",col=2,lwd=2,lty=2)

#
ab<-2*aw;
points(aw,tiwb(aw,ab,hw,hb),type="l",col=3,lwd=2,lty=2)

#
ab<-4*aw;
points(aw,tiwb(aw,ab,hw,hb),type="l",col=4,lwd=2,lty=2)

#
points(aw,tiw(aw,hw),type="l",col=1,lwd=1,lty=3)

legend("topright",col=c(1,2,3,4,1),lwd=c(2,2,2,2,1),lty=c(2,2,2,2,3),bty="n",title="Relações de abundância",
       legend=c("larvas=0.5*besouros","larvas=besouros","larvas=2*besouros","larvas=4*besouros","Somente larvas"))

dev.off()



