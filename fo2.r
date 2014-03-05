eim<-function(em,es,am,eh,hm) {
    return(em-es/am-eh*hm)
}

eir<-function(er,es,ar,eh,hr) {
    return(er-es/ar-eh*hr)
}

eimr<-function(em,am,ar,er,eh,hm,hr) {
    return( em*am/(am+ar)+er*ar/(am+ar)-es/(am+ar)-eh*(hm*am/(am+ar)+hr*ar/(am+ar)) )
}


tim<-function(am,hm) {
    return(1/am+hm)
}

tir<-function(am,hr) {
    return(1/am+hr)
}


timr<-function(am,ar,hm,hr) {
    return(1/(am+ar)+hm*am/(am+ar)+hr*ar/(am+ar))
}

hm<-6
hr<-120

em<-150
er<-2000

es<-1.0
eh<-5.0

am<-seq(0.01,0.40,by=0.01)

ar<-0.1*am

jpeg("01.jpg")
plot(0,0,xlim=c(0,0.5),ylim=c(0,1500),type="n",xlab="Abundância da presa",ylab="Ganho líquido de energia por presa",
     frame=F)
points(am,eim(em,es,am,eh,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("topright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()

jpeg("02.jpg")
plot(0,0,xlim=c(0,0.5),ylim=c(0,250),type="n",xlab="Abundância da presa",ylab="Tempo em segundos",frame=F)
points(am,tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("topright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()

jpeg("03.jpg")
plot(0,0,xlim=c(0,0.5),ylim=c(0,15),type="n",xlab="Abundância da presa",ylab="Ganho líquido de energia por segundo",
     frame=F)
points(am,eim(em,es,am,eh,hm)/tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr)/tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr)/timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("topright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()

jpeg("04.jpg")
layout(matrix(c(1,2,3),ncol=1,nrow=3))
par(mar=c(1, 4, 4, 2) + 0.1)

plot(0,0,xlim=c(0,0.4),ylim=c(0,1500),type="n",xaxt="n",xlab="",ylab="Ganho líquido de energia",frame=F,
     main="Energia por item")
points(am,eim(em,es,am,eh,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(1, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,250),type="n",xaxt="n",xlab="",ylab="Tempo em segundos",frame=F,main="Tempo")
points(am,tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(5, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,15),type="n",xlab="Abundância da presa",ylab="Ganho líquido de energia por segundo",
     frame=F,main="Energia pelo tempo")
points(am,eim(em,es,am,eh,hm)/tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr)/tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr)/timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("bottomright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()

ar<-0.5*am

jpeg("05.jpg")
layout(matrix(c(1,2,3),ncol=1,nrow=3))
par(mar=c(1, 4, 4, 2) + 0.1)

plot(0,0,xlim=c(0,0.4),ylim=c(0,1500),type="n",xaxt="n",xlab="",ylab="Ganho líquido de energia",frame=F,
     main="Energia por item")
points(am,eim(em,es,am,eh,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(1, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,250),type="n",xaxt="n",xlab="",ylab="Tempo em segundos",frame=F,main="Tempo")
points(am,tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(5, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,15),type="n",xlab="Abundância da presa",ylab="Ganho líquido de energia por segundo",
     frame=F,main="Energia pelo tempo")
points(am,eim(em,es,am,eh,hm)/tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr)/tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr)/timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("bottomright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()

ar<-am

jpeg("06.jpg")
layout(matrix(c(1,2,3),ncol=1,nrow=3))
par(mar=c(1, 4, 4, 2) + 0.1)

plot(0,0,xlim=c(0,0.4),ylim=c(0,1500),type="n",xaxt="n",xlab="",ylab="Ganho líquido de energia",frame=F,
     main="Energia por item")
points(am,eim(em,es,am,eh,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(1, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,250),type="n",xaxt="n",xlab="",ylab="Tempo em segundos",frame=F,main="Tempo")
points(am,tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
axis(1,at=seq(0,0.4,0.1),label=NA)

par(mar=c(5, 4, 4, 2) + 0.1)
plot(0,0,xlim=c(0,0.4),ylim=c(0,15),type="n",xlab="Abundância da presa",ylab="Ganho líquido de energia por segundo",
     frame=F,main="Energia pelo tempo")
points(am,eim(em,es,am,eh,hm)/tim(am,hm),type="l",col=1,lwd=2,lty=1)
points(am,eir(er,es,ar,eh,hr)/tir(am,hr),type="l",col=2,lwd=2,lty=1)
points(am,eimr(em,am,ar,er,eh,hm,hr)/timr(am,ar,hm,hr),type="l",col=3,lwd=2,lty=2)
legend("bottomright",col=1:3,lwd=2,lty=c(1,1,2),legend=c("Rato","Coelho","Ambos"),bty="n")
dev.off()
