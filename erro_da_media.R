#http://recologia.com.br/2015/07/intervalo-de-confianca-e-erro-da-media
##################################
## Gerando dados
##################################
set.seed(1)
universo<-rnorm(1000000,5,2)

mean(universo)
sd(universo)

amostra<-sample(universo,30)

mean(amostra)
sd(amostra)

##################################
## Intervalo de confiança
##################################
#jpeg("01.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)
#dev.off()

##################################
## Amostras do universo
##################################
#jpeg("02.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)

for(i in seq(0.05,0.95,length=100)){
    unidade<-sample(universo,1)
    if((unidade>mean(amostra)-1.96*sd(amostra)) & (unidade<mean(amostra)+1.96*sd(amostra))){
        points(i,unidade,pch=19,col="blue",cex=0.5)
    } else {
        points(i,unidade,pch=19,col="red",cex=0.5)
    }
}
#dev.off()

##################################
## Muitos intervalos de confiança
##################################
#jpeg("03.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)

for(i in seq(0.05,0.95,length=100)){
    unidade<-sample(universo,1)
    if((unidade>mean(amostra)-1.96*sd(amostra)) & (unidade<mean(amostra)+1.96*sd(amostra))){
        points(i,unidade,pch=19,col="blue",cex=0.5)
    } else {
        points(i,unidade,pch=19,col="red",cex=0.5)
    }
}

media<-mean(amostra)
amostra_inicial<-amostra
j=2
for(i in seq(1.05,1.95,length=30)) {
    amostra<-sample(universo,30)
    media[j]<-mean(amostra)
    j=j+1
    points(i,mean(amostra),pch=19)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
           length = 0.05, angle = 90)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
           length = 0.05, angle = 90)
}
#dev.off()


##################################
## Intervalo de confiança real
##################################
#jpeg("04.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)

for(i in seq(0.05,0.95,length=100)){
    unidade<-sample(universo,1)
    if((unidade>mean(amostra)-1.96*sd(amostra)) & (unidade<mean(amostra)+1.96*sd(amostra))){
        points(i,unidade,pch=19,col="blue",cex=0.5)
    } else {
        points(i,unidade,pch=19,col="red",cex=0.5)
    }
}

media<-mean(amostra)
amostra_inicial<-amostra
j=2
for(i in seq(1.05,1.95,length=30)) {
    amostra<-sample(universo,30)
    media[j]<-mean(amostra)
    j=j+1
    points(i,mean(amostra),pch=19)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
           length = 0.05, angle = 90)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
           length = 0.05, angle = 90)
}

points(2.05,mean(universo),pch=19,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)-1.96*sd(universo),
       length = 0.02, angle = 90,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)+1.96*sd(universo),
       length = 0.02, angle = 90,col="green")
#dev.off()


##################################
## Erro da média
##################################
#jpeg("05.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)

for(i in seq(0.05,0.95,length=100)){
    unidade<-sample(universo,1)
    if((unidade>mean(amostra)-1.96*sd(amostra)) & (unidade<mean(amostra)+1.96*sd(amostra))){
        points(i,unidade,pch=19,col="blue",cex=0.5)
    } else {
        points(i,unidade,pch=19,col="red",cex=0.5)
    }
}

media<-mean(amostra)
amostra_inicial<-amostra
j=2
for(i in seq(1.05,1.95,length=30)) {
    amostra<-sample(universo,30)
    media[j]<-mean(amostra)
    j=j+1
    points(i,mean(amostra),pch=19)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
           length = 0.05, angle = 90)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
           length = 0.05, angle = 90)
}

points(2.05,mean(universo),pch=19,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)-1.96*sd(universo),
       length = 0.02, angle = 90,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)+1.96*sd(universo),
       length = 0.02, angle = 90,col="green")

points(1.98,mean(media),pch=19,col="darkgray",cex=0.8)
arrows(x0=1.98,y0=mean(media),y1=mean(media)-1.96*sd(media),
       length = 0.02, angle = 90,col="darkgray")
arrows(x0=1.98,y0=mean(media),y1=mean(media)+1.96*sd(media),
       length = 0.02, angle = 90,col="darkgray")
#dev.off()


##################################
## Erro da média
##################################
#jpeg("06.jpg")
plot(0,mean(amostra),pch=19,xlim=c(0,2.1),ylim=c(-2,12),frame=F,
     ylab="Universo",xlab="")
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
       length = 0.05, angle = 90)
arrows(x0=0,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
       length = 0.05, angle = 90)

for(i in seq(0.05,0.95,length=100)){
    unidade<-sample(universo,1)
    if((unidade>mean(amostra)-1.96*sd(amostra)) & (unidade<mean(amostra)+1.96*sd(amostra))){
        points(i,unidade,pch=19,col="blue",cex=0.5)
    } else {
        points(i,unidade,pch=19,col="red",cex=0.5)
    }
}

media<-mean(amostra)
amostra_inicial<-amostra
j=2
for(i in seq(1.05,1.95,length=30)) {
    amostra<-sample(universo,30)
    media[j]<-mean(amostra)
    j=j+1
    points(i,mean(amostra),pch=19)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)-1.96*sd(amostra),
           length = 0.05, angle = 90)
    arrows(x0=i,y0=mean(amostra),y1=mean(amostra)+1.96*sd(amostra),
           length = 0.05, angle = 90)
}

points(2.05,mean(universo),pch=19,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)-1.96*sd(universo),
       length = 0.02, angle = 90,col="green")
arrows(x0=2.05,y0=mean(universo),y1=mean(universo)+1.96*sd(universo),
       length = 0.02, angle = 90,col="green")

points(1.98,mean(media),pch=19,col="darkgray",cex=0.8)
arrows(x0=1.98,y0=mean(media),y1=mean(media)-1.96*sd(media),
       length = 0.02, angle = 90,col="darkgray")
arrows(x0=1.98,y0=mean(media),y1=mean(media)+1.96*sd(media),
       length = 0.02, angle = 90,col="darkgray")

erro_media<-sd(amostra_inicial)/sqrt(30)
points(2.02,mean(amostra_inicial),pch=19,col="lightgray",cex=0.8)
arrows(x0=2.02,y0=mean(amostra_inicial),y1=mean(amostra_inicial)-1.96*erro_media,
       length = 0.02, angle = 90,col="lightgray")
arrows(x0=2.02,y0=mean(amostra_inicial),y1=mean(amostra_inicial)+1.96*erro_media,
       length = 0.02, angle = 90,col="lightgray")
#dev.off()

##################################
## Exemplo do summary
##################################
erro_media
summary(lm(amostra_inicial~1))


