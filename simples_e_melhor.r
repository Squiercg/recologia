tempo<-seq(0,12,by=2)
N<-c(230,1205,1150,2091,3010,6106,7323)

#figura 1
plot(N~tempo,xlab="Tempo",ylab="Tamanho populacional",frame=F,pch=19)

#modelo de regressão linear
modelo1<-lm(N~tempo)
modelo1

#figura 2
plot(N~tempo,xlab="Tempo",ylab="Tamanho populacional",frame=F,ylim=c(0,8000),pch=19)
curve(coef(modelo1)[1]+coef(modelo1)[2]*x,0,12,add=T,lwd=2,lty=1,col="green")
segments(x0=tempo, y0=N, x1 =tempo , y1 = predict(modelo1))

#modelo de regressão polinomial
modelo2<-lm(N~poly(tempo, degree=6, raw=TRUE))
modelo2

#figura 3
plot(N~tempo,xlab="Tempo",ylab="Tamanho populacional",frame=F,ylim=c(0,8000),pch=19)
curve(coef(modelo2)[1]+coef(modelo2)[2]*x+coef(modelo2)[3]*x^2+coef(modelo2)[4]*x^3+coef(modelo2)[5]*x^4+
      coef(modelo2)[6]*x^5+coef(modelo2)[7]*x^6,0,12,add=T,lwd=2,lty=1,col="blue")
segments(x0=tempo, y0=N, x1 =tempo , y1 = predict(modelo2))

#modelo não linear
modelo3<-nls(N~N0+exp(r*tempo),start=list(N0=1,r=1))
modelo3

#figura 4
plot(N~tempo,xlab="Tempo",ylab="Tamanho populacional",frame=F,ylim=c(0,8000),pch=19)
curve(coef(modelo3)[1]+exp(coef(modelo3)[2]*x),0,12,add=T,lwd=2,col="red")
segments(x0=tempo, y0=N, x1 =tempo , y1 = predict(modelo3))

#figura 5
plot(N~tempo,xlab="Tempo",ylab="Tamanho populacional",frame=F,ylim=c(0,10000),xlim=c(0,20))
curve(coef(modelo1)[1]+coef(modelo1)[2]*x,0,20,add=T,lwd=2,lty=1,col="green")
curve(coef(modelo2)[1]+coef(modelo2)[2]*x+coef(modelo2)[3]*x^2+coef(modelo2)[4]*x^3+coef(modelo2)[5]*x^4+
      coef(modelo2)[6]*x^5+coef(modelo2)[7]*x^6,0,20,add=T,lwd=2,lty=1,col="blue")
curve(coef(modelo3)[1]+exp(coef(modelo3)[2]*x),0,20,add=T,lwd=2,col="red")
points(N~tempo,pch=19,cex=1.2)
legend("topleft",col=c("green","blue","red"),lwd=2,bty="n",title ="Modelo",
       legend=c("Regressão Linear","Polinomial de grau 6","Crescimento populacional"))

logLik(modelo1)
logLik(modelo2)
logLik(modelo3)
