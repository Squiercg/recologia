#Post
#http://recologia.com.br/2015/01/crescimento-populacional-com-atraso-para-o-efeito-de-dependencia-da-densidade

#Parâmetros de entrada
N0 <- 1 #População inicial
r  <- 0.1 #Taxa de crescimento Populacional
K  <- 100 #Capacidade Suporte
t  <- 200 #Número de gerações
tau<- 20  #Atraso de resposta para o efeito dependente de densidade

simu_loglag<-function(N0=1,r=0.1,K=100,t=200,tau=10) {
    generation = seq(0,t,by=1)
    dNdt = rep(0,0,t)
    population = rep(0,0,t)

    for(i in 1:length(generation)) {
        #Aqui é o inicio da simulação, como não temos nada de população
        #ainda, usamos a população inicial
        if(i == 1){
            population[i] <- N0
            dNdt[i] <- r*population[i]*(1-population[i]/K)
        } else {
            if(i <= tau) {
                #Aqui, é o caso onde o lag ainda é maior que o tamanho da
                #populacao, lembre-se que se a gente tem um lag de 10, até o
                #tempo 10, não da para olhar 10 tempos atrás, diminuir 10 do
                #i, então a gente tem que usar o crescimento sem lag no
                #começo
                population[i] <- population[i-1]+dNdt[i-1]
                dNdt[i] = r*population[i]*(1-population[i]/K)
             } else {
                 population[i] = population[i-1]+dNdt[i-1]
                 dNdt[i] = r*population[i]*(1-population[i-tau]/K)
             }
        }
    }
    saida<-data.frame(generation,population,dNdt)
    return(saida)
}


simu_lag10<-simu_loglag(tau=10)

#
plot(population~generation,xlab="Tempo",ylab="Tamanho da população",
     data=simu_lag10,type="l",lwd=2,frame=F)

#
plot(dNdt~population,xlab="Tamanho da população",
     ylab="Crescimento populacional",data=simu_lag10,type="l",lwd=2,frame=F)

#
plot((dNdt/population)~population,data=simu_lag10,type="l",lwd=2,frame=F,
     xlab="Tamanho da população",ylab="Crescimento populacional per capita")

#Lag = 10
layout(matrix(1:3,ncol=1,nrow=3))
plot(population~generation,xlab="Tempo",ylab="Tamanho da população",
     data=simu_lag10,type="l",lwd=1,frame=F,
     main="Tamanho populacional ao longo do tempo, Lag = 10")
plot(dNdt~population,xlab="Tamanho da população",
     ylab="Crescimento populacional",data=simu_lag10,type="l",lwd=1,frame=F,
     main="Crescimento vs Tamanho populacional")
plot((dNdt/population)~population,data=simu_lag10,type="l",lwd=1,frame=F,
     xlab="Tamanho da população",ylab="Crescimento populacional per capita"
     ,main="Crescimento per capita vs Tamanho populacional")

#Lag = 3
simu_lag3<-simu_loglag(tau=3)
layout(matrix(1:3,ncol=1,nrow=3))
plot(population~generation,xlab="Tempo",ylab="Tamanho da população",
     data=simu_lag3,type="l",lwd=1,frame=F,
     main="Tamanho populacional ao longo do tempo, Lag = 3")
plot(dNdt~population,xlab="Tamanho da população",
     ylab="Crescimento populacional",data=simu_lag3,type="l",lwd=1,frame=F,
     main="Crescimento vs Tamanho populacional")
plot((dNdt/population)~population,data=simu_lag3,type="l",lwd=1,frame=F,
     xlab="Tamanho da população",ylab="Crescimento populacional per capita"
     ,main="Crescimento per capita vs Tamanho populacional")

#Lag = 20
simu_lag20<-simu_loglag(tau=20)
layout(matrix(1:3,ncol=1,nrow=3))
plot(population~generation,xlab="Tempo",ylab="Tamanho da população",
     data=simu_lag20,type="l",lwd=1,frame=F,
     main="Tamanho populacional ao longo do tempo, Lag = 20")
plot(dNdt~population,xlab="Tamanho da população",
     ylab="Crescimento populacional",data=simu_lag20,type="l",lwd=1,frame=F,
     main="Crescimento vs Tamanho populacional")
plot((dNdt/population)~population,data=simu_lag20,type="l",lwd=1,frame=F,
     xlab="Tamanho da população",ylab="Crescimento populacional per capita"
     ,main="Crescimento per capita vs Tamanho populacional")
