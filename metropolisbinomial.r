#Semente para replicar os resultado do post.
set.seed(51)

#Gerando uma amostra de dados
probabilidade<-0.70
meusdados<-rbinom(n=14, size=1, prob=probabilidade)
meusdados

#Definindo a função de likelihood binomial.
likelihood <- function( theta , data ) {
  z <- sum( data == 1 )
  N <- length( data )
  pDataGivenTheta <- theta^z * (1-theta)^(N-z)
  pDataGivenTheta[ theta > 1 | theta < 0 ] <- 0
  return( pDataGivenTheta )
}

plot(seq(0.01,0.99,0.01),likelihood(seq(0.01,0.99,0.01),meusdados),frame=F,
     xlab="Probabilidade",ylab="Likelihood",type="l")

#Função para definir o prior p(D),
prior <- function( theta ) {
  prior <- dbeta(theta,1,1)
  #exemplo de um prior bimodal
  #prior < dbeta( pmin(2*theta,2*(1-theta)),2,2 )
  prior[ theta > 1 | theta < 0 ] <- 0
  return( prior )
}

plot(seq(0.01,0.99,0.01),prior(seq(0.01,0.99,0.01)),frame=F,
     xlab="Probabilidade",ylab="Likelihood",type="l")

#Função auxiliar para ajudar a calcular a probabilidade relativa
targetRelProb <- function( theta , data ) {
  targetRelProb <-  likelihood( theta , data ) * prior( theta )
  return( targetRelProb )
}

#Exemplos
targetRelProb(0.4,meusdados)/targetRelProb(0.7,meusdados)
targetRelProb(0.5,meusdados)/targetRelProb(0.7,meusdados)
targetRelProb(0.55,meusdados)/targetRelProb(0.7,meusdados)

targetRelProb(0.1,meusdados)/targetRelProb(0.2,meusdados)

targetRelProb(0.6,meusdados)/targetRelProb(0.5,meusdados)

targetRelProb(0.8,meusdados)/targetRelProb(0.9,meusdados)

# Tamanho da cadeia
Ncadeia = 100000

#Inicializando um vetor para salvar os resultados do MCMC
cadeia = rep( 0 , Ncadeia )

# Iniciando o MCMC com um valor arbritario
cadeia[1] = runif(1,min=0,max=1)

# Especificando um valor de burnning, ou seja o quanto do inicio da cadeia vamos jogar fora
# Aqui vamo jogar fora 10%
burnIn = ceiling( .1 * Ncadeia )

# Vamos contar quantas vezes aceitamos ou não os valores propostos
nAceito = 0
nRejeitado = 0

# Agora iniciando o "Random Walk"
for ( t in 1:(Ncadeia-1) ) {
    #Valor Atual
    valoratual = cadeia[t]
    #Propomos uma quantidade para alterar esse valor
    valorproposto = rnorm( 1 , mean = 0 , sd = 0.1 )
    #Computamos a chance de aceitar a mudança
    #Essa chance é igual a Likelihood*prior com o novo valor de theta
    #dividido pele Likeliood*prior antigo
    chanceaceitar = min( 1,
        targetRelProb( valoratual + valorproposto, meusdados ) /
        targetRelProb( valoratual , meusdados )
        )
    #Agora aqui a gente ve na sorte se aceitamos ou não o novo valor.
    #Veja que temos uma chance de fazer a mudança ou manter o ultimo valor
    #Se a gente sortei um número qualquer de 0 a 1 e compara se ele é
    #menor que a chance de aceitar, dessa forma mudamos numa probabilidade
    #da chance de mudar
    if ( runif(1) < chanceaceitar ) {
        #Se aceitarmos mudamos ele na cadeia
        cadeia[ t+1 ] <- valoratual + valorproposto
        #So vamos começar a contar depois do burnIn que escolhemos
        #Afinal, com o começo arbitrario, esperamos aceitar bastante
        #a não ser que escolhemos um valor proximo do final no começo
        if ( t > burnIn ) { nAceito <- nAceito + 1 }

        } else {
            # Quando não aceitamos o valor proposto mantemos o anterior
            cadeia[ t+1 ] <- valoratual
            # E incrementamos aqui apos o BurnIn, como acima
            if ( t > burnIn ) { nRejeitado <- nRejeitado + 1 }
	}
}

#Agora vamos olhar como ficou a cadeia final, ou seja, tirando os valores iniciais
cadeiafinal <- cadeia[ (burnIn+1) : length(cadeia) ]

#Alguns Graficos

#-----------------------------------------------------------------------
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
    sortedPts = sort( sampleVec )
    ciIdxInc = floor( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
        ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
}


#------------------------------------------------------------------------
intconf<-HDIofMCMC(cadeiafinal)

hist(cadeiafinal,ylab="Frequência na cadeia final",xlab="Probabilidades",xlim=c(0,1))
lines(c(intconf[1],intconf[2]),c(500,500),lwd=4,lty=3,col="red")
abline(v=mean(cadeiafinal),lwd=4,col="red")
abline(v=probabilidade,lwd=2,col="blue")

legend("topleft",lty=c(3,1,1),lwd=c(4,4,2),bty="n",cex=0.8,col=c("red","red","blue"),
       legend=c("Intervalo de confiança","Média das estimativas","Probabilidade usada nas estimativas"))

#
plot(density(cadeiafinal),ylab="Frequência",xlab="Probabilidades",xlim=c(0,1),frame=F)
lines(c(intconf[1],intconf[2]),c(0.25,0.25),lwd=3,lty=2,col="red")
abline(v=mean(cadeiafinal),lwd=1,col="red")
abline(v=probabilidade,lwd=1,col="blue")

legend("topleft",lty=c(3,1,1),lwd=c(2,1,1),bty="n",cex=0.8,col=c("red","red","blue"),
       legend=c("Intervalo de confiança","Média das estimativas","Probabilidade usada nas estimativas"))

#
layout(matrix(c(1,2,3,4),ncol=2))
hist(cadeia[1:100],ylab="Frequência na cadeia",xlab="Probabilidades",main="100 primeiros valores",xlim=c(0,1))
plot(0,0,xlim=c(0,100),ylim=c(0,1),type="n",frame=F,main="Cadeia",
     xlab="Posição na cadeia",ylab="Probabilidade")
points(cadeia[1:100],type="l")

hist(cadeia[10000:10500],ylab="Frequência na cadeia",xlab="Probabilidades",main="Do 10000 ao 10500",xlim=c(0,1))
plot(0,0,xlim=c(0,500),ylim=c(0,1),type="n",frame=F,main="Cadeia",
     xlab="Posição na cadeia",ylab="Probabilidade",xaxt="n")
axis(1,at=seq(0,500,by=50),labels=seq(10000,10500,by=50))
points(cadeia[10000:10500],type="l")

# Evidencia para o modelo, p(D).

media<-mean(cadeiafinal)
desvio<-sd(cadeiafinal)

a =   media   * ( (media*(1-media)/desvio^2) - 1 )
b = (1-media) * ( (media*(1-media)/desvio^2) - 1 )

wtdEvid = dbeta( cadeiafinal , a , b ) /
    ( likelihood( cadeiafinal , meusdados ) * prior( cadeiafinal ) )
pData = 1 / mean( wtdEvid )

format(pData,scientific = F)
