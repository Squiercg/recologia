set.seed(171)
preditor<-runif(30,10,30)
resposta<-rnorm(30,2+0.5*preditor,0.5)

plot(resposta~preditor)

############################
#lm do stats
############################
fit.lm <- lm(resposta~preditor)
summary(fit.lm)

############################
#bbmle
############################
library(bbmle)

funcao.likelihood <- function(alpha=1,beta=1,desvio=1) {
    -sum(dnorm(resposta,mean=alpha+beta*preditor,sd=desvio,log=TRUE))
}

fit.mle <- mle2(funcao.likelihood)
summary(fit.mle)
warnings()

fit.mle2<-mle2(funcao.likelihood,method="L-BFGS-B",lower=c(alpha=0.0001,beta=0.0001,desvio=0.0001))
summary(fit.mle2)

fit.mle3<-mle2(funcao.likelihood,optimizer="nlminb",lower=c(alpha=0.0001,beta=0.0001,desvio=0.0001))
summary(fit.mle3)

############################
#OpenBugs
############################
library(R2OpenBUGS)

sink("linreg.txt")
cat("
model {
# Prior
 alpha ~ dnorm(0,0.001)
 beta ~ dnorm(0,0.001)
 sigma ~ dunif(0, 100)
 tau <- 1/ (sigma * sigma)

# Verossimilhança
 for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
 }
}
",fill=TRUE)
sink()

bugs.data <- list(x=preditor,y=resposta,n=length(resposta))

# Função de inicialização
inits <- function(){ list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1))}

# Escolha os parametros que quer estimar
params <- c("alpha","beta","sigma")

# Caracteristicas do MCMC
nc = 3 #Numero de cadeias
ni=1200 #Tamanho da cadeira
nb=200 #Numero de simulação que serão descartadas
nt=1 #Thinning rate

# Inicie o Amostrador

modelo1.bugs <-bugs(data = bugs.data, inits = inits, parameters = params,model = "linreg.txt",n.thin = nt,
                     n.chains = nc,n.burnin = nb, n.iter = ni)

print(modelo1.bugs, dig = 3)

############################
#Jags
############################
library(rjags)

jags <- jags.model(file='linreg.txt',data=bugs.data,inits=inits,n.chains = 3,n.adapt=100)
update(jags, 1000)
jags.samples(jags,c('alpha', 'beta','sigma'),1000)


############################
#Stan
############################
library(rstan)

linreg_stan <- '
data{
  int n;
  real y[n];
  real x[n];
}

parameters{
  real alpha;
  real beta;
  real<lower=0, upper=100> sigma;
}

transformed parameters{
    real mu[n];
    for(i in 1:n){
    mu[i] <- alpha + beta*x[i];
  }
}

model{
  //Priors
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  sigma ~ uniform(0, 100);

  //Likelihood
  for(i in 1:n){
    y[i] ~ normal(mu[i], sigma);
}
}
'

stan.data <- list(n = length(resposta), x = preditor, y = resposta)

fit.stan<- stan(model_code=linreg_stan,data=stan.data,iter = 1000,chains = 3,thin = 1)
print(fit.stan,digits=3)
