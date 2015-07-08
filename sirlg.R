##################################
## Grafo
##################################
library(igraph)

SIR_grafo<-graph.formula(S-+I,I-+R,Entrada-+S,Saida+-S,Saida+-I,Saida+-R)
E(SIR_grafo)$label<-c(expression(paste(beta,"IS")),
                      "m",
                      expression(paste(gamma,"I")),
                      "m",
                      "m",
                      "b")
loc<-matrix(c(0,1,1,1,2,1,0,1.5,1,0.5),ncol=2,byrow=T)

#jpeg("01.jpg")
plot(SIR_grafo,layout=loc,rescale=F,xlim=c(0,2.2),ylim=c(0,2),vertex.size=23)
#dev.off()

##################################
## Modelo
##################################
SIRbd <- function(t,y,p){
    S <- y[1]
    I <- y[2]
    R <- y[3]
    with(as.list(p), {
                    dS.dt <- b * (S+I+R) - B * I * S - m * S
                    dI.dt <- B * I * S - g * I - m * I
                    dR.dt <- g * I -m * R
                    return(list(c(dS.dt,dI.dt,dR.dt)))
                })
}

##################################
## Parametros
##################################
N <- 10^6
R <- 0
I <- 1
S <- N - I - R
g<-1/(13/365)
b <- 1/50
idade <- 5
R0 <- 1+1/(b*idade)
B <- R0 * (g+b)/N

##################################
## Figura
##################################
parms <-c(B=B, #Taxa de transmissão
          g=g, #Taxa de criação de resistencia
          b=b, #Natalidade
          m=b) #Mortalidade

anos <- seq(0,30,by=0.1)

library(deSolve)
SIRbd.out <- data.frame(ode(c(S=S,I=I,R=R),anos,SIRbd,parms,hmax=0.01))

#jpeg("02.jpg")
matplot(SIRbd.out[,1],sqrt(SIRbd.out[,-1]),type="l",col=1,lty=1:3,
        ylab="sqrt(No. of indivíduos)",xlab="Anos",frame=F)
legend("right",c("S","I","R"),lty=1:3,bty="n")
#dev.off()
