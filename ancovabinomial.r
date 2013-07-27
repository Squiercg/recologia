###############################################################
#    ANCOVA Binomial                                          #
###############################################################
#Gerando dados para um exemplo
set.seed(51)
quantidade<-round(runif(3, 200, 300))
peso<-round(runif(sum(quantidade),10,80))
hist(peso)
peso<-(peso-mean(peso))/sd(peso)
hist(peso)

#Preditores
dados<-data.frame(local=factor(rep(c("Mata Atlantica", "Amazonia", "Cerrado"),quantidade)),peso)
dados

Xmat <- model.matrix(~ local*peso,data=dados)   #Matriz de Efeitos
print(Xmat, dig = 2)
beta.vec <- c(0, 2, -0.5, 0.5, 1, 0)            #Valor dos parametros

lin.pred <- Xmat[,] %*% beta.vec	        # Valor do preditor Linear
exp.p <- exp(lin.pred) / (1 + exp(lin.pred))    # Chance esperada
exp.p

#Resposta
status <- rbinom(n = nrow(dados), size = 1, prob = exp.p) # Adicionando erro binomial
status				                          # Checando o que foi criado

#Juntando tudo num data.frame
dados<-data.frame(status,dados)

##################################################################

#Olhando os dados
dados

#Grafico dos Diagnósticos
#jpeg("01.jpg")
barplot(table(dados$status),ylim=c(0,500),xlab="Diagnóstico",ylab="Número de passarinhos")
#dev.off()
#Grafico dos diagnósticos por locais
#jpeg("02.jpg")
barplot(table(dados$status,dados$local),ylim=c(0,300),xlab="Diagnóstico",ylab="Número de passarinhos",beside=T,
        legend.text = c("Não parasitado","Parasitado"))
#dev.off()
#Grafico dos diagnósticos por locais e peso.
library(lattice)

panel.smoother <- function(x, y) {
  panel.xyplot(x, y,pch=19,col="black")             # Plota os pontos
  panel.loess(x, y,span=0.6,lwd=2,lty=2,col="red")  # Mostra a linha que muda de acordo com a média
}

#jpeg("03.jpg")
xyplot(status~peso|local,data=dados,panel=panel.smoother,layout = c(1, 3))
#dev.off()

#Grafo do modelo
library(igraph)
#jpeg("04.jpg")
plot(graph.formula( Local -+ Status , Peso -+ Status , Local ++ Peso ),vertex.size=25,edge.color="black",
     vertex.color="white")
#dev.off()

### Analise usando OpenBugs
# Definindo um modelo
sink("ancovabin.txt")
cat("
model {

# Priors
 for (i in 1:n.local) {
    alpha[i] ~ dnorm(0, 0.01)  # Interceptos (Um pra cada local)
    beta[i] ~ dnorm(0, 0.01)   # Inclinações (Um pra cada local)
 }

# Likelihood
 for (i in 1:n) {
    status[i] ~ dbin(p[i], 1)
    logit(p[i]) <- alpha[local[i]] + beta[local[i]]* peso[i]
 }

# Recuperando os efeitos baseados no caso base "Amazonia"
 a.effe2 <- alpha[2] - alpha[1]		# Intercepto Amazonia vs Cerrado
 a.effe3 <- alpha[3] - alpha[1]		# Intercept Amazonia vs Mata Atlantica
 b.effe2 <- beta[2] - beta[1]		# Inclinação Amazonia vs Cerrado
 b.effe3 <- beta[3] - beta[1]		# Inclinação Amazonia vs Mata Atlantica

# Teste Personalizado (Respondendo a minha pergunta)
 test1 <- alpha[2] - alpha[1]		# Diferença entre Cerrado a Amazonia
 test2 <- alpha[2] - alpha[3]		# Diferença entre Cerrado e Mata Atlantica
}
",fill=TRUE)
sink()

# Juntando os dados numa lista (Veja que o local a gente manda como um número apenas,
#vc tem que lembrar que número é o que)
bugs.data <- list(status = dados$status, local = as.numeric(dados$local), n.local = 3, n = nrow(dados),peso=dados$peso)

# Função para iniciar a cadeia
inits <- function(){ list(alpha = rlnorm(3, 3, 1), beta = rlnorm(3, 2, 1))} # Note log-normal inits
#Ela so gera valores ao acaso para iniciar as cadeias, veja so
inits()

# Parametros que queremos salvar os resultados
params <- c("alpha", "beta","a.effe2","a.effe3","b.effe2","b.effe3","test1","test2")

# Configurações do MCMC
ni <- 1500
nb <- 500
nt <- 5
nc <- 3

# Iniciando o Gibbs Sampler
library(R2OpenBUGS)
out <-bugs(data = bugs.data, inits = inits, parameters.to.save = params, model.file = "ancovabin.txt",
           n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni)

print(out,digits=3)
beta.vec
summary(glm(status ~ local * peso, family = binomial,data=dados))

str(out)

#jpeg("05.jpg")
par(mfrow=c(2,1))
hist(out$sims.list$test1,xlim=c(0,max(out$sims.list$test2)),main="Diferença entre Cerrado a Amazonia",
     xlab="Tamanho da Diferença")
lines(c(0,0),c(0,400),lty=2,lwd=2,col="red")

hist(out$sims.list$test2,xlim=c(0,max(out$sims.list$test2)),main="Diferença entre Cerrado e Mata Atlantica",
     xlab="Tamanho da Diferença")
lines(c(0,0),c(0,400),lty=2,lwd=2,col="red")
#dev.off()
