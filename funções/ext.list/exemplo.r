dados<-matrix(sample(c(0,1),16,replace = T),ncol=4,nrow=4)
dados
probs<-c(1/4,1/4,1/4,1/4)

exemplo<-extlist(matriz=dados,nsim=50,probabilidades=probs)
exemplo
