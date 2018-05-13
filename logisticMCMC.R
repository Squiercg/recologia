##
rm(list=ls())
library(mvtnorm)


##Gerando dados de exemplo
set.seed(123)                                     
n<-250
preditor<-sample(0:4,n,replace=T)	   
intercepto<- -2				                   
inclinacao<-  1
p_log<-intercepto+inclinacao*preditor

prob<-exp(p_log)/(1+exp(p_log))
resposta<-rbinom(n,1,prob)


X<-matrix(c(rep(1,n),preditor), ncol=2)
k<-2


fit<-glm(resposta~preditor,family=binomial)
Beta<-c(0,0)
lambda<-rep(1,n)

nu<-8
nsim<-1000  # Número de amostras
z<-rep(0,n)					                # Latent Normal Variable
cadeia<-matrix(0,nrow=nsim,ncol=k)  # 

###################
## Gibss Sampler ##
###################

i=2
for (i in 2:nsim) {

    ##Update z
    muz<-X%*%Beta
    z[resposta==0]<-qnorm(runif(n,0,pnorm(0,muz,sqrt(1/lambda))),muz,sqrt(1/lambda))[resposta==0]
    z[resposta==1]<-qnorm(runif(n,pnorm(0,muz,sqrt(1/lambda)),1),muz,sqrt(1/lambda))[resposta==1]

    ##Update Beta
    W<-diag(lambda)
    vbeta<-solve(t(X)%*%W%*%X)
    betahat<-vbeta%*%(t(X)%*%W%*%z)  	
    cadeia[i,]<-c(rmvnorm(1,betahat,vbeta))
    Beta<-cadeia[i,]

    ##Update Lambda
    lambda<-rgamma(n,(nu+1)/2,scale=2/(nu+(z-X%*%Beta)^2))
    if (i%%100==0) print(i)
}

## Results
mean.beta<-colMeans(cadeia/.65)
mean.beta

plot(1:nsim,Betamat[,2]/.634, type="l",col="lightgreen")
abline(h=mean.beta[2],col="blue4")



