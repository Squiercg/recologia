set.seed(250)

#######
#Dados#
#######

n<-100
mu.real<-5
sigma2.real<-2
tau.real<-1/sigma2.real
dados<-rnorm(n,mu.real,sqrt(sigma2.real))

a<-0.001
b<-0.001

###########
# Priors  #
###########
mu.prior<-0
sigma2.prior<-10
tau.prior<-1/sigma2.prior
mtau<-a+n/2

####################
# Inicio da cadeia #
####################

mu<-0.1
tau<-0.1
nsim<-5000

##########
# Cadeia #
##########
cadeia.mu<-c(mu,rep(0,nsim-1))
cadeia.tau<-c(tau,rep(0,nsim-1))
A<-0


#Valores para avaliar a maxima verossimilhança
valores<-seq(-5,15,by=0.1)
likelihood<-vector()

for(i in 1:length(valores)) {
    likelihood[i]<-prod(dnorm(dados,valores[i],sqrt(2),log=F))
    }

plot(likelihood~valores,type="b",frame=F,ylab="likelihood")

loglikelihood<-vector()

for(i in 1:length(valores)) {
    loglikelihood[i]<-sum(dnorm(dados,valores[i],sqrt(2),log=T))
    }

plot(loglikelihood~valores,type="b",frame=F,ylab="loglikelihood")

exp(loglikelihood)[100]==likelihood[100]
all.equal(exp(loglikelihood)[100],likelihood[100])

max(likelihood)

which(likelihood==max(likelihood))
valores[101]
mean(dados)
valores[102]

max(loglikelihood)
which(loglikelihood==max(loglikelihood))


############
#   MCMC   #
############
for (t in 2:nsim){
    mus<-mu+0.3*rt(1,3)

    r<- (sum(dnorm(dados,mus,1/sqrt(tau),log=T) - dnorm(dados,mu,1/sqrt(tau),log=T)) +
         dnorm(mus,mu.prior,sqrt(sigma2.prior),log=T) -  dnorm(mu,mu.prior,sqrt(sigma2.prior),log=T))

    if (log(runif(1))<r) {
        mu<-mus
        A<-A+1
        }

    cadeia.mu[t]<-mu

    tau<-rgamma(1,mtau,b+sum((dados-mu)^2)/2)
    cadeia.tau[t]<-tau
}

###########
# Gráfico #
###########

# Iteration Plots
layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,4,5,6,7,8),nrow=3,byrow=T))

plot(seq(1,nsim) ,cadeia.mu[1:nsim],type="l",xlab="Iteration", ylab="mu",frame=F,
main="Média",col=2,lty=1,ylim=c(0,10))
abline(h=mu.real)
plot(seq(1,nsim) ,cadeia.tau[1:nsim],type="l",xlab="Iteration", ylab="mu",frame=F,
main="Tau",col=2,lty=1,ylim=c(0,1))
abline(h=1/sigma2.real)

par(mar=c(2,2,2,2),font=3,cex=0.7)

hist(cadeia.mu[1:100],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(3,6),breaks=seq(0,6,by=0.02))
hist(cadeia.mu[2000:2500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(3,6),breaks=seq(3,6,by=0.02))
hist(cadeia.mu[4500:5000],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(3,6),breaks=seq(3,6,by=0.02))
hist(cadeia.tau[1:500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1),breaks=seq(0,1,by=0.01))
hist(cadeia.tau[2000:2500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1),breaks=seq(0,1,by=0.01))
hist(cadeia.tau[4500:5000],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1),breaks=seq(0,1,by=0.01))

plot(cadeia.mu[1:500],sqrt(1/cadeia.tau[1:500]),type="b",frame=F,xlab="Média",ylab="Desvio Padrão",cex=0.5,pch=19,
     xlim=c(0,6),ylim=c(0,6))
library(MASS)
contornos<-kde2d(cadeia.mu[1:500],sqrt(1/cadeia.tau[1:500]))
contour(contornos,add=T,lwd=2,nlevels = 6,col=rev(heat.colors(6)))
