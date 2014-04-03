#Codigo original
#http://freakonometrics.blog.free.fr/index.php?post/2011/12/20/Basic-on-Markov-Chain-(for-parents)
#Informações sobre o jogo
#http://en.wikipedia.org/wiki/Snakes_and_Ladders

#Primeiro defina a matrix de transição.
#Tem que definir como vai mudando as coisas, definindo todas as
#Escadinhas que te jogam pra frente e que cobrinhas que te jogam pra traz.

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


#Vamos entender o que foi feito.
#O tabuleiro inteiro é composto de 100 quadradinhos
nrow(M)
#Mas temo o momento da primeira parada de dados, O, ou seja
#seu pião ta fora do tabuleiro, vc vai joga o dado e ai vai
#parar em algum lugar do tabuleiro

#Agora qd vc joga o dado a primeira vez o que pode acontecer?
M[1,]
#Temos que entender que:
#Vc pode mudar para 6 lugares, com chance igual de cada lugar
1/6
# onde são esses lugares?
M[1,]
#No 1, 2 ,3, 5, 6 ,14
#a parada é que no se cair 4 no dado vc para no 4
#definimos isso ali em cima, ollha  a linha  starting, qd ta 4
#o ending ta 14

#Agora vamos supor vc vai faze sua segunda jogada
#que vc tirou um na primeira jogada e esta na posição 2,
#onde vc pode ir para na proxima jogada?
M[2,]
#Olha que o 2 ja ta 0, ou seja,essa matrix te diz onde vc pode ir parar
#e como é um numero entre 1 e 6 vc tem sempre 6 lugares, e a matrix te da
#a chance de onde vc vai estar dependendo da onde vc esta agora
#ou seja o numero da linha é o lugar que vc esta
#são 101 colunas que são os lugares que vc pode estar, no tempo

#Muito legal, mas qual a importancia dessa porra?
#Com uma markov chain vc integra qualquer pergunta.
#se vc sabe como o sistema muda, vc pode perguntar muitas coisas
#calcular uma respostas

#vamos fazer uma função pra somar possibilidades
powermat=function(P,h) {
    Ph=P
    if(h>1) {
        for(k in 2:h) {
            Ph=Ph%*%P
        }
    }
    return(Ph)
}

#estabelecer o inicio do jogo, que é no 0
initial=c(1,rep(0,n))

#fazer uma variação de cores, com 101 possibilidades
COLOR=rev(heat.colors(101))

#Fazer um dataframe pra um plot bonito
u=1:sqrt(n)
boxes=data.frame(index=1:n,ord=rep(u,each=sqrt(n)),abs=rep(c(u,rev(u)),sqrt(n)/2))

#Função para plotar o calculo das posições
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









#Agora vamos perguntar o seguinte, onde vc vai estar apos a primeira
#parada de dados, jogou o dado uma vez, onde vc vai estar?
position(1)

#tudo ta da mesma cor, pq vc pode estar  em 6 lugares so, e eles
#estão marcadinhos ae
#agora vc jogou o a segunda vez
position(2)
#agora a porra muda de figura
#note que o 7 é o lugar mais vermelhinho pq?
#pois vc jogou o dado 2 vezes, e lembra que
#7 é a soma mais comum?
#6+1,2+5,3+4... e assim vai
#agora se vc tirou 4 na primeira jogada, vc subiu a escadinha para o 14
#ai se tirou 6 na segunda, vc para no 20 e sobe a escada pro 38, olhe como
#é bem clarinho a cor, ou seja como é especifico as 2 paradas de dado
#é pouco provavel vc estar ali, comparado com o 7 por exemplo

#aqui vamo faze um loop para ver como o jogo rola ao longo de
#100 jogadas
for (i in 1:100) {
                  position(i)
                  Sys.sleep(1)
                  }


#Mas imagina que vc bica a mesa e derruba seu peão
#e não lembra onde estava?
#vamos olhar para 10 jogadas onde vc pode estar?
position(10)

#se vc fala que estava em algum lugar 59 60 ou 61
#podemos ver a probabilidade de estar em cada lugar
h=10
posição<-(initial%*%powermat(M,h))[59:61]/sum((initial%*%powermat(M,h))[59:61])
#de 100% de possibilidades
sum(posição)
#eles tão distribuidas assim
posição
#ou seja, é mais facil provavel que vc tivesse no quadrinho 60, que o 61
#e dificilmente no 59


#podemos ainda olhar a distribuição de ainda estar jogando
#pelo tanto de jogadas

distrib=initial%*%M
game=rep(NA,1000)
for(h in 1:length(game)){
game[h]=distrib[n+1]
distrib=distrib%*%M}
plot(1-game[1:200],type="l",lwd=2,col="red",
ylab="Probabilidade de ainda estar jogando")

#olha so la pela jogada 50, ja vai te acabando o jogo
#depois de 100 jogada, cabou, ta 0 a chance de gente ta jogando
position(50)

#Com essa distribuição, vc pode perguntar qual
#o numero de jogadas pra acabar o jogo

sum(1-game)

#Ou seja, em 33 jogadas em média, o jogo acaba, mas em 50% dos jogos
#leva menos de 29 jogadas

max(which(1-game>.5))
position(29)

#Mas como ninguem joga sozinho, e o jogo acaba qd alguem chega no 100
#podemos perguntar quando o jogo acaba qd algum de nois 2 chega a 100


plot((1-game[1:200])^2,type="l",lwd=2,col="blue",
ylab="Probabilidade de ainda estar jogando (2 jogadores)")

#E o numero esperado de jogadas para acaba o jogo com 2 jogadores
#Here, the expected number of turns before ending the game is

sum((1-game)^2)

#e com 3 jogadores?

plot((1-game[1:200])^3,type="l",lwd=2,col="purple",
ylab="Probabilidade de ainda estar jogando (3 jogadores)")

#Quanto mais jogador, mais rapido o jogo acaba

sum((1-game)^3)



