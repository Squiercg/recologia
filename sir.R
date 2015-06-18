SIR <- function(t, y, p) {
    {
    S <- y[1]
    I <- y[2]
    R <- y[3]
    }
    with(as.list(p), {
                    dS.dt <- -B * I * S
                    dI.dt <- B * I * S - g * I
                    dR.dt <- g * I
                    return(list(c(dS.dt, dI.dt, dR.dt)))
                })
}

N <- 10000
I <- 1
R <- 1
S <- N - I - R
parms <- c(B = 0.01, g = 4)
meses <- seq(0, 1.5, by = 0.01)

#install.packages("deSolve")
library(deSolve)
SIR.out <- data.frame(ode(c(S, I, R), meses, SIR, parms))
colnames(SIR.out)<-c("tempo","S","I","R")

matplot(meses, SIR.out[, -1], type = "l", lty = 1:3,frame=F,
        xlab="Tempo em Meses",ylab="N",lwd=2,col=2:4)
legend("right", c("Resistentes", "Infectados", "Susceptiveis"), lty = 3:1, col = c(4,3,2), bty = "n",lwd=2)
