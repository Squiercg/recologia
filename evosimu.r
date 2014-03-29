# **************************************************
# Condições iniciais
# **************************************************

N0      <- 1000 # População inicial
rate_A  <- 1.2  # Taxa de crescimento do alelo "A"
rate_a  <- 1.2  # Taxa de crescimento do alelo "a"
fA      <- 0.3  # Frequência do alelo "A"
max_gen <- 20   # Número de gerações a simular

# **************************************************
# Calculando variáveis derivadas
# **************************************************

fa   <- 1.0 - fA  # Frequência do alelo "a"
NA_0 <- N0 * fA   # População inicial do alelo "A"
Na_0 <- N0 * fa   # Polulação inicial do alelo "a"

# **************************************************
# Simulação
# **************************************************

evosimu<-function(N0,rate_A,rate_a,fA,max_gen) {
    fa <-1.0-fA
    NA_0<- N0 * fA
    Na_0<- N0 * fa

    resultado<-matrix(NA,ncol=5,nrow=max_gen+1)
    colnames(resultado)<-c("Nt","NA_t","Na_t","fA_t","fa_t")
    resultado[1,]<-c(N0,NA_0,Na_0,fA,fa)

    for(t in 2:(max_gen+1)) {
        resultado[t,"NA_t"] = NA_0 * (rate_A ^ t)
        resultado[t,"Na_t"] = Na_0 * (rate_a ^ t)
        resultado[t,"Nt"] =  resultado[t,"NA_t"] + resultado[t,"Na_t"]
        resultado[t,"fA_t"] =  resultado[t,"NA_t"] / resultado[t,"Nt"]
        resultado[t,"fa_t"] = resultado[t,"Na_t"] / resultado[t,"Nt"]
    }
    return(resultado)
}

evosimu(N0=N0,rate_A=rate_A,rate_a=rate_a,fA=fA,max_gen=10)

saida<-evosimu(N0=N0,rate_A=rate_A,rate_a=rate_a,fA=fA,max_gen=max_gen)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")

#N0 = 50     rate_A = 1.2  rate_a = 1.2  fA = 0.3
saida<-evosimu(N0=50,rate_A=1.2,rate_a=1.2,fA=0.3,max_gen=20)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")

#N0 = 1000   rate_A = 0.7  rate_a = 0.7  fA = 0.3
saida<-evosimu(N0=100,rate_A=0.7,rate_a=0.7,fA=0.3,max_gen=max_gen)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topright",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")

#N0 = 1000   rate_A = 2.0  rate_a = 1.5  fA = 0.02
saida<-evosimu(N0=1000,rate_A=2.0,rate_a=1.5,fA=0.02,max_gen=max_gen)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")

#N0 = 1000   rate_A = 1.2  rate_a = 0.9  fA = 0.02
saida<-evosimu(N0=1000,rate_A=1.2,rate_a=0.9,fA=0.02,max_gen=max_gen)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")

#N0 = 10000  rate_A = 0.8  rate_a = 0.6  fA = 0.02
saida<-evosimu(N0=10000,rate_A=0.8,rate_a=0.6,fA=0.02,max_gen=max_gen)

layout(matrix(c(1,2),nrow=2,ncol=1))
plot(0:max_gen,saida[,"Nt"],frame=F,xlab="Geração",ylab="População",type="l",ylim=c(0,max(saida[,"Nt"])))
points(0:max_gen,saida[,"NA_t"],type="l",col="red")
points(0:max_gen,saida[,"Na_t"],type="l",col="blue")
legend("topleft",col=c("black","red","blue"),lty=1,legend=c("Total","A","a"),bty="n")
plot(0:max_gen,saida[,"fA_t"],frame=F,xlab="Geração",ylab="Frequência Gênica",type="l",ylim=c(0,1),col="red")
points(0:max_gen,saida[,"fa_t"],type="l",col="blue")
