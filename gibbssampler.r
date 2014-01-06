# Original:  http://people.duke.edu/~neelo003/r/Normal%20Mean%20Gibbs.r
# Gibbs sampler para uma distribuição normal
###################################
set.seed(250)

########
# Data #
########
n<-1000
media.real<--2
desvio.padrao.real<-2
dados<-rnorm(n,media.real,desvio.padrao.real)

hist(dados,main="Histograma",ylab="Frequência",xlab="Valores")
abline(v=mean(dados),lty=2,col="red",lwd=2)

#####################
# Valores do Prior  #
#####################
media.prior<-5
tau.prior<-0.1

####################
# Valores iniciais #
####################
tau<-1
mtau<-n/2

####################################
# Vetor para guardar os resultados #
####################################

nsim<-5000
cadeia.media<-rep(0,nsim)
cadeia.variancia<-rep(0,nsim)

#################
# Gibbs Sampler #
#################
vc<-rep(0,nsim)
mc<-rep(0,nsim)
tauc<-rep(0,nsim)


for (i in 1:nsim) {

  v<-1/(tau*n + tau.prior)
  m<-v*(tau*sum(dados)+tau.prior*media.prior)
  cadeia.media[i]<-rnorm(1,m,sqrt(v))

  tau<-rgamma(1,mtau,sum((dados-cadeia.media[i])^2)/2)
  cadeia.variancia[i]<-1/tau

}

library(igraph)
grafo<-graph.formula(n-+v,tau-+v,tau.prior-+v,v-+m,dados-+m,tau.prior-+m,media.prior-+m,m-+cadeia.media,v-+cadeia.media,
                   cadeia.media-+tau,dados-+tau,tau-+cadeia.variancia,tau-+m)
tkplot(grafo,vertex.size=60,vertex.color="gray",vertex.label.color="black",edge.arrow.size=2,edge.color="black",
       vertex.label.cex=1.2)

grafo<-graph.formula(m-+cadeia.media,v-+cadeia.media,cadeia.media-+tau,tau-+cadeia.variancia,tau-+m,tau-+v)
tkplot(grafo,vertex.size=60,vertex.color="gray",vertex.label.color="black",edge.arrow.size=2,edge.color="black",
       vertex.label.cex=1.2)

layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T))
curve(dgamma(x,shape=1,rate=0.01),0,10,main="Rate = 0.01",frame=F)
curve(dgamma(x,shape=1,rate=0.1),0,10,main="Rate = 0.1",frame=F)
curve(dgamma(x,shape=1,rate=1),0,10,main="Rate = 1",frame=F)
curve(dgamma(x,shape=1,rate=10),0,10,main="Rate = 10",frame=F)

##########################
# Distribuição Posterior #
##########################
mean(cadeia.media[1501:nsim])
sd(cadeia.media[1501:nsim])

sqrt(mean(cadeia.variancia[1501:nsim]))
sd(cadeia.variancia[1501:nsim])


#Evolução da cadeia
plot(1:nsim,cadeia.media,type="l",col="red",frame=F)
abline(h=mean(cadeia.media),lwd=2,lty=2)
abline(v=1500,lwd=4,lty=3,col="gray")
text(750,-1.76,"Burn-in")

#Distribuição posterior
plot(cadeia.media[1:500],sqrt(cadeia.variancia[1:500]),type="b",frame=F,xlab="Média",ylab="Desvio Padrão",cex=0.5,pch=19)
library(MASS)
contornos<-kde2d(cadeia.media[1:500],sqrt(cadeia.variancia[1:500]))
contour(contornos,add=T,lwd=4,nlevels = 6,col=rev(heat.colors(6)))
