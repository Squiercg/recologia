##http://recologia.com.br/2016/10/duas-especies-em-metapopulacoes-de-levins/
##install.packages("deSolve")
library(deSolve)
rm(list=ls())

##Função
metpop2sp <- function(t, y, p) {
    {
        o1 <-y[1] 
        o2 <-y[2]
    }
    with(as.list(p), {
        do1.dt <- c1*o1*(1-o1)-e*o1
        do2.dt <- c2*o2*(1-o1-o2)-c1*o1*o2-e*o2
        return(list(c(do1.dt, do2.dt)))
    })
}

##Tempo
tempo <- seq(0, 200, by =5)

##Caso 1
c1 = 0.4
c2 = 0.5 
o1 = 0.1
o2 = 0.4 
e = 0.25

out.ode<-ode(c(o1,o2), tempo, metpop2sp, list(c1=c1,c2=c2,e=e))

##jpeg("01.jpg")
matplot(out.ode[,1], out.ode[, -1], type = "l", lty = 1:2,frame=F,xlab="Tempo",ylab="N",lwd=2,col=c("red","blue"))
legend("right", c("Sp1", "Sp2"), lty = 1:2, col = c("red","blue"), bty = "n",lwd=2)
##dev.off()

##Caso 2
c1 = 0.1 
c2 = 0.1
o1 = 0.05 
o2 = 0.05 
e = 0.05

out.ode<-ode(c(o1,o2), tempo, metpop2sp, list(c1=c1,c2=c2,e=e))
##jpeg("02.jpg")
matplot(out.ode[,1], out.ode[, -1], type = "l", lty = 1:2,frame=F,xlab="Tempo",ylab="N",lwd=2,col=c("red","blue"))
legend("right", c("Sp1", "Sp2"), lty = 1:2, col = c("red","blue"), bty = "n",lwd=2)
##dev.off()

##Caso 3
c1 = 0.05
c2 = 0.07 
o1 = 0.05 
o2 = 0.05
e = 0.06

out.ode<-ode(c(o1,o2), tempo, metpop2sp, list(c1=c1,c2=c2,e=e))
##jpeg("03.jpg")
matplot(out.ode[,1], out.ode[, -1], type = "l", lty = 1:2,frame=F,xlab="Tempo",ylab="N",lwd=2,col=c("red","blue"))
legend("right", c("Sp1", "Sp2"), lty = 1:2, col = c("red","blue"), bty = "n",lwd=2)
##dev.off()

##Caso 4
c1 = 0.1
c2 = 0.5
o1 = 0.05 
o2 = 0.05
e = 0.05


out.ode<-ode(c(o1,o2), tempo, metpop2sp, list(c1=c1,c2=c2,e=e))
##jpeg("04.jpg")
matplot(out.ode[,1], out.ode[, -1], type = "l", lty = 1:2,frame=F,xlab="Tempo",ylab="N",lwd=2,col=c("red","blue"))
legend("right", c("Sp1", "Sp2"), lty = 1:2, col = c("red","blue"), bty = "n",lwd=2)
##dev.off()
