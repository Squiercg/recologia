#Codigo original
#http://freakonometrics.blog.free.fr/index.php?post/2011/12/20/Basic-on-Markov-Chain-(for-parents)

#Matrix de transição
n=100
M=matrix(0,n+1,n+1+6)
rownames(M)=0:n
colnames(M)=0:(n+6)

for(i in 1:6) {
    diag(M[,(i+1):(i+1+n)])=1/6
}

M[,n+1]=apply(M[,(n+1):(n+1+6)],1,sum)
M=M[,1:(n+1)]
starting=c(4,9,17,20,28,40,51,54,62,64,63,71,93,95,92)
ending  =c(14,31,7,38,84,59,67,34,19,60,81,91,73,75,78)

for(i in 1:length(starting)) {
    v=M[,starting[i]+1]
    ind=which(v>0)
    M[ind,starting[i]+1]=0
    M[ind,ending[i]+1]=M[ind,ending[i]+1]+v[ind]
}

#Olhando o que criamos
nrow(M)
M[1,]
M[2,]

sum(M[2,]>0)
sum(M[10,]>0)
sum(M[50,]>0)

#Função para multiplicar a matriz de transição
powermat=function(P,h) {
    Ph=P
    if(h>1) {
        for(k in 2:h) {
            Ph=Ph%*%P
        }
    }
    return(Ph)
}

#Função para plotar o calculo das posições
initial=c(1,rep(0,n))
COLOR=rev(heat.colors(101))
u=1:sqrt(n)
boxes=data.frame(index=1:n,ord=rep(u,each=sqrt(n)),abs=rep(c(u,rev(u)),sqrt(n)/2))

#
position=function(h){
    D=initial%*%powermat(M,h)
    plot(0:10,0:10,col="white",axes=FALSE,xlab="",ylab="",main=paste("Posição após",h,"jogadas"))

    for(i in 1:n) {
        polygon(boxes$abs[i]-c(0,0,1,1),boxes$ord[i]-c(0,1,1,0),col=COLOR[min(1+trunc(500*D[i+1]),101)],border=NA)
    }

    segments(c(0,10),rep(0,2),c(0,10),rep(10,2))
    segments(rep(0,2),c(0,10),rep(10,2),c(0,10))
    segments(0:10,rep(0,11),0:10,rep(10,11))
    segments(rep(0,11),0:10,rep(10,11),0:10)

    for(i in 1:length(starting)) {
        arrows(x0=boxes$abs[starting[i]]-0.5,
               y0=boxes$ord[starting[i]]-0.5,
               x1=boxes$abs[ending[i]]-0.5,
               y1 =boxes$ord[ending[i]]-0.5,
               lty=3,length=0.10,col="darkgray",lwd=2)
    }
    text(boxes$abs-.5,boxes$ord-.5,boxes$index,cex=.7)
}

#figuras

position(1)
position(2)
position(10)

#Se quiser ver uma animação olhe esse codigo, no Sys.sleep da para configurar
#uma pausa entre os plots, deixei em 1/4 de segundo por jogada.
#for (i in 1:100) {
#                  position(i)
#                  Sys.sleep(0.25)
#                  }


#Probabilidade parcial
h=10
posição<-(initial%*%powermat(M,h))[59:61]/sum((initial%*%powermat(M,h))[59:61])
sum(posição)
posição

#Distribuição de jogadas
distrib<-initial%*%M
game<-rep(NA,1000)

for(h in 1:length(game)){
    game[h]<-distrib[n+1]
    distrib<-distrib%*%M
}

#Figuras de distribuição
plot(1-game[1:200],type="l",lwd=2,col="red",ylab="Probabilidade de ainda estar jogando",xlab="Jogadas")
position(50)

sum(1-game)


max(which(1-game>.5))
position(29)

#Distribuição para dois jogadores
plot((1-game[1:200])^2,type="l",lwd=2,col="blue",ylab="Probabilidade de ainda estar jogando (2 jogadores)",
     xlab="Jogadas")
sum((1-game)^2)

#Distribuição para multiplos jogadores.
plot((1-game[1:200])^1,type="l",lwd=2,lty=2,col=1,ylab="Probabilidade de ainda estar jogando",xlab="jogadas")
points((1-game[1:200])^2,type="l",lwd=2,lty=2,col=2)
points((1-game[1:200])^3,type="l",lwd=2,lty=2,col=3)
points((1-game[1:200])^4,type="l",lwd=2,lty=2,col=4)
legend("topright",legend=c(1,2,3,4),col=1:4,lwd=2,lty=2,bty="n",title="Número de Jogadores")
