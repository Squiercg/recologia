##http://recologia.com.br/2017/12/metodo-de-monte-carlo-e-o-experimento-da-agulha-de-buffon/

##Ilustrando o problema de buffon
b=1
a=0.3


plot(0,0,type="n",xlim=c(0,4*b),ylim=c(0,4*b),frame=F,xlab="",ylab="",main="Piso",xaxt="n",yaxt="n")
abline(v=seq(0,3*b,b))
polygon(x=c(0,1,1,0),y=c(-1,-1,5,5),col="brown3")
polygon(x=c(2,3,3,2),y=c(-1,-1,5,5),col="brown3")


for(i in 1:100){
    xcenter <- runif(1,0,4*b)
    ycenter <- runif(1,0,4*b)

    phi <- runif(1,0,2*pi)
    x0 <- xcenter-(a/2)*cos(phi)
    y0 <- ycenter-(a/2)*sin(phi)
    x1 <- xcenter+(a/2)*cos(phi)
    y1 <- ycenter+(a/2)*sin(phi)

    points(x=c(x0,x1),y=c(y0,y1),type="l")
    points(x=x0,y=y0,type="p",pch=19,cex=0.6)
}

##Tamanho das agulhas
sqrt((x1-x0)^2+(y1-y0)^2)

##Testando se ela encostou no canto
xcenter%%(b/2)-(a/2)*cos(phi%%(pi/2))


##Experimento simplificado
n <- 2000
b <- 1
a <- 0.7
nhits <- 0
for(i in 1:n){
    xcenter <- runif(1,0,b/2)
    phi <- runif(1,0,pi/2)
    xtip <- xcenter-(a/2)*cos(phi)
    if(xtip<0){
        nhits <- nhits+1
        }
}

##Porcentagem de hits
nhits/n

##Estimativa de quanto deve ser a porcentagem de hits
((a/b)*(2/pi))


##Avaliando a relação entre tamanho do piso, tamanho da agulha e hits
b_vetor<- seq(0.1,b,length.out = 6)
a_vetor<- seq(0,1,0.001)

par(mfrow=c(3,2))
for(i in 1:length(b_vetor)){
    plot(a_vetor, ((a_vetor/b_vetor[i])*(2/pi)),type="l",xlab="Valores de a",ylab="N hits",main=paste("b=",round(b_vetor[i],2),sep=""),ylim=c(0,5))
}

b_vetor<- seq(0.1,b,0.001)
a_vetor<- seq(0.1,1,length.out = 6)
par(mfrow=c(3,2))
for(i in 1:length(a_vetor)){
    plot(b_vetor, ((a_vetor[i]/b_vetor)*(2/pi)),type="l",xlab="Valores de b",ylab="N hits",main=paste("a=",round(a_vetor[i],2),sep=""),ylim=c(0,5))
}


##Plotando as duas variáveis juntas
angulo <- seq(0,pi/2,length.out = 500)
vetor <- seq(0,b/2,length.out = 500)
casos <- expand.grid(angulo=angulo,b=vetor)
casos$cor <- ifelse(casos$b<a/2 & casos$angulo < acos(casos$b/(a/2)),"lightblue","brown1")


plot(casos$b,casos$angulo,col=casos$cor,pch=19,cex=0.4,xaxt="n",yaxt="n",xlab="",ylab="",frame=F)
axis(1,at=c(0,a/2,b/2),labels=c(0,"a/2","b/2"))
axis(2,at=c(0,pi/4,pi/2),labels=c(0,expression(phi),expression(frac(pi,2))),las=2)
text(a/4,pi/8,expression('N'['hits']*'=1'))
text(a/2,pi/3.5,expression('N'['hits']*'=0'))
