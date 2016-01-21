set.seed(123)
library(mvtnorm)

rm(list=ls())

## Simulando dados
## Tamanho da amostra
n<-250
## Preditor                                      
x<-runif(n)
b0<- -5 #intercepto
b1<-  2 #inclinaçãoa

eta<-b0+b1*x #preditor
p<-exp(eta)/(1+exp(eta)) #equação logistica
y<-rbinom(n,1,p) #observações
X<-matrix(c(rep(1,n),x), ncol=2)



Beta<-c(0,0)
lambda<-rep(1,n) ##peso
nu<-8					                       

## Store Results
nsim<-1000  			                   
z<-rep(0,n)					               
Betamat<-matrix(0,nrow=nsim,ncol=2) 

###################
## GIBBS SAMPLER	  #
###################

for (i in 2:nsim) {
    ## UPDATE Z
    muz<-X%*%Beta
    ## Update Mean of Z
    z[y==0]<-qnorm(runif(n,0,pnorm(0,muz,sqrt(1/lambda))),muz,sqrt(1/lambda))[y==0]
    z[y==1]<-qnorm(runif(n,pnorm(0,muz,sqrt(1/lambda)),1),muz,sqrt(1/lambda))[y==1]

    ## UPDATE BETA
    W<-diag(lambda)
    vbeta<-solve(t(X)%*%W%*%X)
    betahat <- vbeta%*%(t(X)%*%W%*%z)  	# Note: this isn't best code -- 
    
    Beta<-c(rmvnorm(1,betahat,vbeta))
    Betamat[i,]<-Beta

    ## UPDATE LAMBDA
    lambda<-rgamma(n,(nu+1)/2,scale=2/(nu+(z-X%*%Beta)^2))
    if (i%%100==0) print(i)
}

                                        # Results
mean.beta<-colMeans(Betamat/.634) # Correction factor is 1/.634
mean.beta

plot(1:nsim,Betamat[,2]/.634, type="l",col="lightgreen")
abline(h=mean.beta[2],col="blue4")


## Ajuste 
fit<-glm(y~x,family=binomial)
fit
