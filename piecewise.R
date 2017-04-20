set.seed(31415)

nutriente <- runif(60,0,10)

fn_bs <- function(x){
    saida<-vector(length=length(x))
    for(i in 1:length(x)){       
        if(x[i]<5){
            saida[i] <- 8+0*x[i]
        }else{
            saida[i] <- 1+2*x[i]
        }
    }
    saida <- saida+rnorm(length(x),0,1)
    return(saida)
}

tamanho <- fn_bs(nutriente)

jpeg("01.jpg")
plot(tamanho~nutriente,xlab="Concentração de nutrientes",ylab="Tamanho da plantula (cm)",frame=F,ylim=c(0,30),xlim=c(0,12),pch=19)
dev.off()

#####################################
## Força bruta
#####################################
quebras <- seq(1,9,0.5)
minimo_quadrado <- vector(length = length(quebras))

for(i in 1:length(quebras)){
 piecewise <- nls(tamanho ~ ifelse(nutriente < quebras[i],a1+b1*nutriente,a2+b2*nutriente),start = c(a1=1,a2=1,b1=1,b2=1))
 minimo_quadrado[i] <- logLik(piecewise)
}


minimo_quadrado <- unlist(minimo_quadrado)

jpeg("02.jpg")
plot(quebras,minimo_quadrado,type="b",pch=19,xlab="Valor de quebra",ylab="LogLikelihood",frame=F,xlim=c(0,10))
dev.off()

modelo<-nls(tamanho ~ ifelse(nutriente < 5,a1+b1*nutriente,a2+b2*nutriente),start = c(a1=1,a2=1,b1=1,b2=1))
summary(modelo)

modelo_linear<- lm(tamanho~nutriente)
summary(modelo_linear)


jpeg("03.jpg")
plot(tamanho~nutriente,xlab="Concentração de nutrientes",ylab="Tamanho da plantula (cm)",frame=F,ylim=c(0,30),xlim=c(0,12),pch=19)

abline(modelo_linear,lwd=3,lty=3,col="blue")

curve(coef(modelo)[1]+coef(modelo)[3]*x,0,5,add=T,lwd=3,lty=3,col="red")
curve(coef(modelo)[2]+coef(modelo)[4]*x,5,12,add=T,lwd=3,lty=3,col="red")
abline(v=5,lwd=1,lty=2,col="red")
legend("topleft",lwd=3,lty=3,col=c("blue","red"),legend=c("Modelo Linear","Modelo piecewise"),bty="n")
dev.off()


jpeg("04.jpg")
par(mfrow=c(2,1))
plot(resid(modelo_linear),main=paste("Loglikelihood do modelo liner =",round(logLik(modelo_linear),3)),frame=F,xlab="Amostras",ylab="Resíduo",ylim=c(-5,5))
abline(h=0,lty=2,lwd=2)
plot(resid(modelo),main=paste("Loglikelihood do modelo piecewise =",round(logLik(modelo),3)),frame=F,xlab="Amostras",ylab="Resíduo",ylim=c(-5,5))
abline(h=0,lty=2,lwd=2)
dev.off()

##install.packages("segmented")
library(segmented)


modelo_pieciwise<- segmented(modelo_linear, seg.Z = ~nutriente, psi=1)
summary(modelo_pieciwise)


jpeg("05.jpg")
plot(tamanho~nutriente,xlab="Concentração de nutrientes",ylab="Tamanho da plantula (cm)",frame=F,ylim=c(0,30),xlim=c(0,12),pch=19)
plot(modelo_pieciwise,add=T,lwd=2,lty=2,col="red")
dev.off()

