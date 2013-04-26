#Criando dados, abrindo pacotes, etc
library(R2OpenBUGS)
set.seed(15)
dose<-factor(rep(c("Controle","Dose1","Dose2","Dose3"),each=15))
medidas<-rnorm(60,rep(c(2,2,3,4),each=15))

#Grafico dos dados
boxplot(medidas~dose,frame.plot=F,ylim=c(0,6),ylab="Medidas",xlab="Dose")

# Escrevendo o modelo para o OpenBugs
sink("anova.txt")
cat("
model {
  # Priors
  for (i in 1:4){
    alpha[i] ~ dnorm(0, 0.001)
    }
  sigma ~ dunif(0,100)
  # Likelihood
  for (i in 1:60) {
    medidas[i] ~ dnorm(mean[i], tau)
    mean[i] <- alpha[dose[i]]
    }
  # Quantidades Derivadas
  tau <- 1 / ( sigma * sigma)
  }
  "
,fill=TRUE)
sink()

# Juntando os dados.
bugs.data <- list(medidas=medidas,dose=as.numeric(dose))
# Gerador alearorio de valores iniciais
inits <- function(){ list(alpha = rnorm(4, mean = mean(medidas)), sigma = rlnorm(1) )}
# Parametros a serem estimados
params <- c("alpha","sigma")
# Configurações do MCMC
ni <- 1200
nb <- 200
nt <- 2
nc <- 3

# Começando o Gibbs MCMC
out<-bugs(data=bugs.data,inits=inits, parameters.to.save=params,
model.file="anova.txt",n.thin=nt,n.chains=nc,n.burnin=nb,n.iter=ni)
# Olhando o resultado, restringindo a 3 digitos
print(out, dig = 3)

#Estatistica Rhat
out$summary[,"Rhat"]

#Verificando se todos ficaram menor que 1.1
all(out$summary[,"Rhat"] < 1.1)

#Salvando as cadeias inteiras: codaPkg=TRUE
out<-bugs(data=bugs.data,inits=inits, parameters.to.save=params,
model.file="anova.txt",n.thin=nt,n.chains=nc,n.burnin=nb,n.iter=ni,
          codaPkg=TRUE)

out
out.coda <- read.bugs(out)

#Abrindo o pacote coda
library(coda)

#Olhando as cadeias inteiras
xyplot(out.coda)

#Densidade dos parametros estimados
densityplot(out.coda)

#Procurando autocorrelação
acfplot(out.coda)

#Shrinkage factor
gelman.diag(out.coda)
gelman.plot(out.coda)

#Retirando estimativas dos parametros
out.summary <- summary(out.coda, q=c(0.025, 0.975))
out.summary$stat
out.summary$q
