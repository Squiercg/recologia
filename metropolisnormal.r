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

mu<-runif(1,0,10)
tau<-runif(1,0,10)
nsim<-5000

##########
# Cadeia #
##########
cadeia.mu<-rep(0,nsim)
cadeia.tau<-rep(0,nsim)
A<-0

############
#   MCMC   #
############
for (t in 1:nsim){
    mus<-mu+0.3*rt(1,3)

    r<- (sum(dnorm(dados,mus,1/sqrt(tau),log=T) - dnorm(dados,mu,1/sqrt(tau),log=T)) +
         dnorm(mus,mu.prior,sqrt(sigma2.prior),log=T) -  dnorm(mu,mu.prior,sqrt(sigma2.prior),log=T))

    if (log(runif(1))<r) {
        mu<-mus
        A<-A+1
        }

    cadeia.mu[t]<-mu
    tau<-cadeia.tau[t]<-rgamma(1,mtau,b+sum((dados-mu)^2)/2)
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
main="Desvio",col=2,lty=1,ylim=c(0,1))
abline(h=1/sigma2.real)

par(mar=c(2,2,2,2),font=3,cex=0.7)

hist(cadeia.mu[1:100],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(4,6))
hist(cadeia.mu[2000:2500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(4,6))
hist(cadeia.mu[4500:5000],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(4,6))
hist(cadeia.tau[1:500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1))
hist(cadeia.tau[2000:2500],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1))
hist(cadeia.tau[4500:5000],xlab="",ylab="Frequência",main="",cex=0.7,xlim=c(0,1))

