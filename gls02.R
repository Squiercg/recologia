library(ape)
library(R2OpenBUGS)
library(MASS)
library(nlme)

#Read tree
tr <- "((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galago:1.00);"
tree.primates <- read.tree(text = tr)

#Read data
X <- c(4.09434, 3.61092, 2.37024, 2.02815, -1.46968)
Y <- c(4.74493, 3.33220, 3.36730, 2.89037, 2.30259)
dados<-data.frame(X,Y)
rownames(dados)<-tree.primates$tip.label

plot(Y~X)
abline(a=-2.300993,b=1.262139)
abline(a=2.5,b=0.3)

#Run example from ape
compar.gee(X ~ Y, phy = tree.primates)
modelo.gls<-gls(X ~ Y,corr=corBrownian(phy=tree.primates),data=dados)
summary(modelo.gls)


#Now the model from the paper
sink("linregevo.txt")
cat("
model {
    #Priors
    alpha ~ dnorm(0,1.0E-06)
    beta ~ dnorm(0,1.0E-06)
    tau ~ dgamma(1,1)
    sigma <- 1 / (sigma * sigma)

    #Linear regression and multivariate normal likelihood
    for (i in 1:Nspec) {
        mu[i] <- alpha+beta*X[i]
        }
    Y[1:Nspec] ~ dmnorm(mu[],TAU[,])

    #Variance-covariance matrix construction
    for (i in 1:Nspec) {
        for (j in 1:Nspec) {
            TAU[i,j] <- tau*invA[i,j]
            }
        }
}
",fill=TRUE)
sink()

# because there is only one tree, we can only get one matrix
cov.mat <- vcv(tree.primates, corr=T)
inv.mat <- ginv(cov.mat)

#Gather the data
bugsd <- list(Y=Y, X=X, Nspec=5, invA=inv.mat)

#Init function
inits <- function(){ list(alpha=rnorm(1), beta=rnorm(1), tau = rgamma(1,1))}

#MCMC configurations
# Caracteristicas do MCMC
nc = 3 #Numero de cadeias
ni=2000 #Tamanho da cadeira
nb=500 #Numero de simulação que serão descartadas
nt=3 #Thinning rate

# Escolha os parametros que quer estimar
params <- c("alpha","beta", "tau")

#Run the model
modelo2.bugs <-bugs(data = bugsd, inits = NULL, parameters = params,
                    model = "linregevo.txt",n.thin = nt,
                     n.chains = nc,n.burnin = nb, n.iter = ni)
print(modelo2.bugs,digits=3)
summary(lm(Y~X))
##############################################

library(R2OpenBUGS)
library(MASS) # para usar mvrnorm
library(MCMCpack) # para usar rwish

# Gerando os daod
n <- 500
medias <-c(1,5)
covariancia <- matrix(c(1,.2,.2,2),nrow=2)

resposta <- mvrnorm(n,medias,covariancia)


plot(resposta,frame=F,xlab="Eixo x",ylab="Eixo y")

sd(resposta[,1])
sd(resposta[,2])

mean(resposta[,1])
mean(resposta[,2])

# Set up for WinBUGS
mu0 = as.vector(c(0,0))
S2 = matrix(c(1,0,0,1),nrow=2)/1000
S3 = matrix(c(1,0,0,1),nrow=2)/10000
data=list("y","N","S2","S3","mu0")
inits=function(){list( mu=mvrnorm(1,mu0,matrix(c(10,0,0,10),nrow=2) ),
tau = rwish(3,matrix(c(.02,0,0,.04),nrow=2)) )}
# Run WinBUGS
multi_norm.sim = bugs(data,inits,model.file="mult_normal.txt",
parameters=c("mu","tau","Sigma"),n.chains = 2,n.iter=4010,
n.burnin=10,n.thin=1,)
print(multi_norm.sim,digits=3)


cat("
model{
    
for(i in 1:N) {
    y[i,1:2] ~ dmnorm(mu[],tau[,])
}

mu[1:2] ~ dmnorm(mu0[],S2[,])
tau[1:2,1:2] ~ dwish(S3[,],3)
Sigma[1:2,1:2] <- inverse(tau[,])

}
", file="mult_normal.txt")



geotree <- read.nexus("Geospiza.nex")
geospiza13.tree<-drop.tip(geotree,"olivacea")
geodata<-read.table("Geospiza.txt",row.names=1)

wingL<-geodata$wingL
tarsusL<-geodata$tarsusL
DF.geospiza<-data.frame(wingL,tarsusL,row.names=row.names(geodata))
DF.geospiza <-  DF.geospiza[geospiza13.tree$tip.label, ]
DF.geospiza

bm.geospiza<-corBrownian(phy=geospiza13.tree)
bm.gls<-gls(wingL~tarsusL,correlation=bm.geospiza,data=DF.geospiza)
summary(bm.gls)

cov.mat <- vcv(geotree, corr=T)
inv.mat <- ginv(cov.mat)

#Gather the data
bugsd <- list(Y=DF.geospiza$wingL, X=DF.geospiza$tarsusL, Nspec=nrow(DF.geospiza), invA=inv.mat)





my.data <- list(N = nrow(DF.geospiza), D = inv.mat, y = DF.geospiza$wingL, X=DF.geospiza$tarsusL)

cat("
model
{
  # priors
      beta ~ dnorm(0, 0.01)
      alpha ~ dnorm(0, 0.01)
      lambda ~ dgamma(1, 0.1) 
      global.mu ~ dnorm(0, 0.01)
      global.tau ~ dgamma(0.001, 0.001)
      for(i in 1:N)
      {
        # vector of mvnorm means mu
        mu[i] ~ dnorm(global.mu, global.tau) 
      }

  # derived quantities
      for(i in 1:N)
      {
          for(j in 1:N) {
      
          # turning the distance matrix to covariance matrix
          D.covar[i,j] <- exp(-lambda*D[i,j])
      }
      }
      # turning covariances into precisions (that's how I understand it)
      D.tau[1:N,1:N] <- inverse(D.covar[1:N,1:N])

  # likelihood
      for (i in 1:Nspec) {
        mu[i] <- alpha+beta*X[i]
        }
      y[1:N] ~ dmnorm(mu[], D.tau[,])
}
", file="mvnormal.txt")
library(R2jags)    # JAGS-R interface
fit <-  jags(data=my.data,
       parameters.to.save=c("alpha", "beta"),
       model.file="mvnormal.txt",
       n.iter=10000,
       n.chains=3,
       n.burnin=5000,
       n.thin=5,
       DIC=FALSE)
