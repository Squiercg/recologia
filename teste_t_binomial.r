set.seed(123)
### Gerando dados
dados<-data.frame(Sp=c("Columbina talpacoti","Scardafella squammata"),N=c(62,57),
                  Prevalence=c(0.516,0.193))

dados$simul<-0

for(i in 1:2) {
    dados[i,"simul"]<-rbinom(1, dados[i,"N"], prob = dados[i,"Prevalence"])
    }

#figura 1
layout(matrix(1:2,ncol=1,nrow=2))
for(i in 1:2) {
    plot(dbinom(x=0:dados$N[i],size=dados$N[i],prob=dados$Prevalence[i])~c(0:dados$N[i]),frame=F,pch=19,
         main=paste(dados$Sp[i]," N=",dados$N[i],sep=""),ylab="Frequência",xlim=c(0,70),xlab="",ylim=c(0,0.15))
    legend("topright",legend=paste("Prevalência =",dados$Prevalence[i]*100,"%"),bty="n")
    lines(c(dados$N[i],dados$N[i]),c(0.05,0),col="blue",lty=2,lwd=3)
    text(dados$N[i]+5,0.05,paste("Limite N =",dados$N[i]),cex=0.7)
    }

tabela<- as.table(cbind(dados$simul, dados$N - dados$simul))
dimnames(tabela) <- list(Sp = c("Columbina talpacoti","Scardafella squammata"),Parasita = c("Sim","Não"))
tabela

teste.chi <- chisq.test(tabela)

str(teste.chi)
teste.chi$observed   # Contagens observadas
teste.chi$expected   # Contagens esperada sobre a hipotese nula
teste.chi$residuals  # Residuo de Pearson
teste.chi$stdres     # Residos "standardized"

### Análise usando Bugs
# Definindo Modelo

sink("Binomial.t.test.txt")
cat("
model {

# Priors
 alpha ~ dnorm(0,0.01)
 beta ~ dnorm(0,0.01)

# Likelihood
 for (i in 1:n) {
    C[i] ~ dbin(p[i], N[i]) 		# Note p before N
    logit(p[i]) <- alpha + beta *species[i]
 }
# Derived quantities
 prev.C_talpacoti <- exp(alpha) / (1 + exp(alpha))
 prev.S_squammata <- exp(alpha + beta) / (1 + exp(alpha + beta))
 prev.Diff <- prev.C_talpacoti - prev.S_squammata	# Teste
}
",fill=TRUE)
sink()

# Juntando os dados numa lista
bugs.dados <- list(C = dados$simul, N = dados$N, species = c(0,1), n = length(dados$Sp))

# Função geradora de parametros iniciais para as cadeias.
inits <- function(){ list(alpha=rlnorm(1), beta=rlnorm(1))}

# Parametros a estimar
params <- c("alpha", "beta", "prev.C_talpacoti", "prev.S_squammata", "prev.Diff")

# Configurações do MCMC
nc <- 3
ni <- 1200
nb <- 200
nt <- 2

# Iniciando o  Gibbs sampling
library(R2OpenBUGS)
out <- bugs(data=bugs.dados, inits=inits, parameters.to.save=params,model.file="Binomial.t.test.txt",
            n.thin=nt, n.chains=nc, n.burnin=nb,n.iter=ni)

print(out, dig = 3)

#Onde estão os dados
out$summary

#Figura 2
str(out)
par(mfrow = c(3,1))
hist(out$sims.list$prev.C_talpacoti, col = "grey", xlim = c(0,1), xlab="",
	main = "Prevalência de C. talpacoti", breaks = 30,ylim=c(0,300))
abline(v = out$mean$prev.C_talpacoti, lwd = 3, col = "red")
hist(out$sims.list$prev.S_squammata, col = "grey", xlim = c(0,1), xlab= "",
	main = "Prevalência de S. squammata", breaks = 30,ylim=c(0,300))
abline(v = out$mean$prev.S_squammata, lwd = 3, col = "red")
hist(out$sims.list$prev.Diff, col = "grey", xlim = c(0,1), xlab = "",
	main = "Diferença nas Prevalências", breaks = 30,ylim=c(0,300))
abline(v = 0, lwd = 3, col = "red")

### Analise usando GLM
modelo.glm<-glm(cbind(dados$simul, dados$N - dados$simul) ~ dados$Sp, family = binomial)
summary(modelo.glm)
