library(untb)
library(ggplot2)
library(reshape)
library(RColorBrewer)
set.seed(123)

data()

###################################################
### Deriva Genética
###################################################
#Original - http://statisticalrecipes.blogspot.com.br/2012/02/simulating-genetic-drift.html

#Parametros da Simulação de Deriva Genetica
N = 20           # Número de individuos diploides
N.chrom = 2*N    # Número de cromossomos
N.gen = 100      # Número de gerações
N.sim = 10       # Número de simulações
p = 0.2
q = 1-p

# Simulation
saida <- matrix(0,nrow=N.gen,ncol=N.sim,dimnames = list(paste("Geraçao",1:N.gen),paste("S",1:N.sim)))
saida[1,] = rep(N.chrom*p,N.sim) # Inicializando o numero de alelos na primeira geração
for(j in 1:N.sim){
  for(i in 2:N.gen){
    saida[i,j]<-rbinom(1,N.chrom,prob=saida[i-1,j]/N.chrom)
    }
  }
#Transformando em frequencia
saida<-data.frame(saida/N.chrom)

# Modificando os dados para a estrutura que da para plotar
sim_data <- melt(saida)
ggplot(sim_data, aes(x = rep(c(1:100), N.sim), y = value, colour = variable)) +
    geom_line() +
    scale_colour_brewer(palette="Set3") +
    ggtitle("Simulação de deriva genética") +
    xlab("Geração") + ylab("Frequencia alélica") +
    ylim(0,1) +
    labs(colour = "Simulações")
#ggsave("01.jpg")

###################################################
### Deriva Ecologica
###################################################
saida <- untb(rep(1:10, 90), prob=0, gens=5000, D=9, keep=TRUE)

sim_data <- melt(data.frame(species.table(saida)))

ggplot(sim_data, aes(x = rep(c(1:5000), 10), y = value, colour = variable)) +
    geom_line() +
    scale_colour_brewer(palette="Set3") +
    ggtitle("Simulação de deriva populacional") +
    xlab("Geração") + ylab("Abundância da espécie") +
    labs(colour = "Simulações")
#ggsave("02.jpg")
