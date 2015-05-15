#http://recologia.com.br/2015/05/calculando-o-valor-p/
##################################
## Gerando dados
##################################
set.seed(123)
tratamento<-factor(rep(c("A","B"),each=30))
resposta<-rnorm(60,as.numeric(tratamento)*0.5,1)

#jpeg("01.jpg")
boxplot(resposta~tratamento,xlab="tratamento")
#dev.off()

#jpeg("02.jpg")
stripchart(resposta~tratamento,vertical=T,method="jitter",pch=19,
           at=c(1.2,1.8),xlab="tratamento")
#dev.off()

##################################
## Calculando estatística t
##################################
A<-resposta[tratamento=="A"]  #Valores de A
B<-resposta[tratamento=="B"]  #Valores de B
n<-nA<-nB<-length(A)          #Tamanho das amostras

#Equal sample sizes, equal variance
t1<-( mean(A)-mean(B) ) /
    ( sqrt(0.5*(var(A)+var(B)))*sqrt(2/n) )
round(t1,digits=4)

#Equal or unequal sample sizes, equal variance
t2<-( mean(A)-mean(B) ) /
  (sqrt( ( (nA-1)*var(A)+(nB-1)*var(B) ) / (nA+nB-2) ) * sqrt(1/nA + 1/nB))
round(t2,digits=4)

#Equal or unequal sample sizes, unequal variances
t3<-( mean(A)-mean(B) )/
    sqrt( var(A)/nA+(var(B)/nB) )
round(t3,digits=4)

#Distribuição t
#jpeg("03.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")

arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

integrate(function(x) dt(x, df=2*n-2),-Inf,Inf)

0.975-0.025

qt(p=c(0.025,0.975), df=2*n-1)
integrate(function(x) dt(x, df=2*n-2),-2.000995,2.000995)

#95%
#jpeg("04.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")

polygon(x= c(seq(-2.000995,2.000995,length=1000) ),
        y= c(0, dt(x=seq(-2.000995,2.000995,length=998), df=2*n-1),0 ),col="red"  )

arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

#99%
0.995-0.005
qt(p=c(0.005,0.995), df=2*n-2)

#jpeg("05.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")

polygon(x= c(seq(-2.661759,2.661759,length=1000) ),
        y= c(0, dt(x=seq(-2.661759,2.661759,length=998), df=2*n-2),0 ),col="red"  )
arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

#jpeg("06.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")
polygon(x= c(seq(t1,-4,length=1000) ),
        y= c(0, dt(x=seq(t1,-4,length=998), df=2*n-2),0 ),col="red")
arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

#valor p
integrate(function(x) dt(x, df=2*n-2),-Inf,t1)
t.test(resposta~tratamento,alternative = "less",var.equal = TRUE)

pt(t1,df=2*n-2)

#invertendo a ordem da pergunta
t1<-( mean(B)-mean(A) ) /
    ( sqrt(0.5*(var(B)+var(A)))*sqrt(2/n) )
round(t1,digits=4)

#jpeg("07.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")
polygon(x= c(seq(t1,4,length=1000) ),
        y= c(0, dt(x=seq(t1,4,length=998), df=2*n-2),0 ),col="red")
arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

#mudando quem é o intercepto
tratamento <- relevel(tratamento, "B")
t.test(resposta~tratamento,alternative = "less",var.equal = TRUE)

#jpeg("08.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")
polygon(x= c(seq(-4,t1,length=1000) ),
        y= c(0, dt(x=seq(-4,t1,length=998), df=2*n-2),0 ),col="red")
arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
#dev.off()

integrate(function(x) dt(x, df=2*n-2),-Inf,t1)
integrate(function(x) dt(x, df=2*n-2),t1,Inf)

#
#jpeg("09.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")
polygon(x= c(seq(t1,4,length=1000) ),
        y= c(0, dt(x=seq(t1,4,length=998), df=2*n-2),0 ),col="red")
polygon(x= c(seq(-4,-t1,length=1000) ),
        y= c(0, dt(x=seq(-4,-t1,length=998), df=2*n-2),0 ),col="red")
arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
arrows(x0=-t1,x1=-t1 ,y0=0.05, y1 = 0,lwd=2,length = 0.10)
text(x=t1,y=0.06,"Valor t calculado")
text(x=-t1,y=0.06,"Menos Valor t calculado")
#dev.off()

#jpeg("10.jpg")
plot(dt(x=seq(-4,4,by=0.01), df=2*n-2)~seq(-4,4,by=0.01),
     type="l",ylab="Probabilidade",xlab="Valor t")
polygon(x= c(seq(t1,4,length=1000) ),
        y= c(0, dt(x=seq(t1,4,length=998), df=2*n-2),0 ),col="red")
polygon(x= c(seq(-4,-t1,length=1000) ),
        y= c(0, dt(x=seq(-4,-t1,length=998), df=2*n-2),0 ),col="red")
abline(v=0,lwd=3,lty=3)
points(c(0,t1),c(0.05,0.05),type="l",lwd=3,lty=3)
points(c(0,-t1),c(0.05,0.05),type="l",lwd=3,lty=3)

arrows(x0=t1,x1=t1 ,y0=0.05, y1 = 0,lwd=3,lty=3,length = 0.10)
arrows(x0=-t1,x1=-t1 ,y0=0.05, y1 = 0,lwd=3,lty=3,length = 0.10)
#dev.off()

integrate(function(x) dt(x, df=2*n-2),-Inf,-t1)
integrate(function(x) dt(x, df=2*n-2),t1,Inf)
integrate(function(x) dt(x, df=2*n-2),-Inf,-t1)$value+integrate(function(x) dt(x, df=2*n-2),t1,Inf)$value

#
pt(-t1,df=2*n-2)
1-pt(t1,df=2*n-2)

#
pt(-t1,df=2*n-2)==(1-pt(t1,df=2*n-2))
all.equal(pt(-t1,df=2*n-2),1-pt(t1,df=2*n-2))

#
2*pt(-abs(t1),df=2*n-2)
t.test(resposta~tratamento,var.equal=TRUE)
t.test(resposta~tratamento)


