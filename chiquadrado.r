set.seed(123)
chi2_moscas<-function(observado) {

    if(is.vector(observado) && length(observado)==2) {

        esperado<-rep(sum(observado)/2,2)
        estatistica<-((observado[1]-esperado[1])^2/esperado[1])+((observado[2]-esperado[2])^2/esperado[2])
        return(estatistica)

    } else {
        print("Entrada errada. Os dados devem ser entrados como vetor numericos de tamanho 2!")
    }

}


chi2_moscas(c(200))
chi2_moscas(c(221,475))
chi2_moscas(c(332,375))


dados<-vector()

for(i in 1:50) {
    dados[i]<-chi2_moscas(c(50-i,50+i))
}

#figura 1
plot(dados,frame=F,type="p",pch=19,cex=0.5,xaxt="n",ylab="Estatisthica Chi-quadrado",xlab="Razão de Machos para Femeas")
axis(1,at=c(1,13,25,38,49),label=c("1:1","1:13","1:25","1:38","1:49"))


sample(c("Macho","Femea"),100,replace=T)

estatistica<-vector()

for(i in 1:10000) {
    experimento<-sample(c("Macho","Femea"),100,replace=T)
    macho<-sum(experimento=="Macho")
    femea<-sum(experimento=="Femea")
    estatistica[i]<-chi2_moscas(c(macho,femea))
}

#figura 2
hist(estatistica,prob=T,main="Estatística Chi-Quadrado",xlab="Estatística")



#figura 3
hist(estatistica,prob=T,main="Estatística Chi-Quadrado",xlab="Estatística")
curve(dchisq(x,1),0,15,add=T,lwd=3,lty=2,col="red")
abline(v=qchisq(0.95,1),col="blue",lty=3)
legend("topright",lwd=c(3,1),col=c("red","blue"),lty=c(2,3),legend=c("Distribuição Chi-Quadrado","Quantile de 95%"),
       bty="n")


sum(estatistica<qchisq(0.95,1))/length(estatistica)


experimento1<-c(43,57)
chi2_moscas(experimento1)
sum(chi2_moscas(experimento1)<estatistica)/length(estatistica)
chisq.test(experimento1,p=c(0.5,0.5))


experimento2<-c(21,79)
chi2_moscas(experimento2)
sum(chi2_moscas(experimento2)<estatistica)/length(estatistica)
chisq.test(experimento2,p=c(0.5,0.5))

format(6.631e-09,scientific=F)

preditor<-rnorm(30)
resposta<-rnorm(30)

modelo1<-lm(resposta~preditor)
modelo2<-lm(resposta~1)

summary(modelo1)
summary(modelo2)

anova(modelo1,modelo2,test="Chisq")
