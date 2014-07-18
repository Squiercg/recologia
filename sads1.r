library(sads)
library(vegan)

data(BCI)
N <- sort(colSums(BCI), decr = TRUE)

#Figura 01
hist(N, main = "Classes de densidade de espécies",ylab="Frequência",xlab="Classes de Abundância")

#Figura 02
hist(log(N), xlab = "Log das Classes de Abundância", main = NULL)
m.spp <- mean(log(N))
sd.spp <- sd(log(N))
R <- length(N)
curve(dnorm(x, m.spp, sd.spp) * R, 0, 8, add = T,col="red",lty=2,lwd=2)

#Figura 03
plot(log(N), pch=19, ylim = c(0, 8), main = NULL,xlab = "Species Rank", ylab = "Log Density",frame=F)
ranks.lognormal <- R * (1 - pnorm(log(N), m.spp,sd.spp))
lines(ranks.lognormal, log(N),lwd=3,lty=2,col="red")
legend("topright",legend="Curva Log-Normal",bty="n", log(N),lwd=3,lty=2,col="red")


N.oc<- octav(N,preston=T)
N.oc

#Figura 04
plot(N.oc)

exp(8)

N.ln <- fitsad(N,"lnorm")
summary(N.ln)

#Figura 05
par(mfrow=c(2,2))
plot(N.ln)
