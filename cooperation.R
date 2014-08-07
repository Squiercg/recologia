# Simulação do torneio de Axelroad
set.seed(1415)

# Retorno para:
# Cooperação mutua
coopera.coopera <- 1
# Você trapaceia mas seu oponente coopera
trapaceia.coopera <- 2
# Você coopera mais seu oponente trapaceia
coopera.trapaceia <- -2
# Todo mundo trapaceia
trapaceia.trapaceia <- -1

# Função que retorna o valor de resultado da interação para o participante
retorno <- function(voce,oponente) {
    saida <- 0*voce
    saida[(voce==1)&(oponente==1)] = coopera.coopera
    saida[(voce==0)&(oponente==1)] = trapaceia.coopera
    saida[(voce==1)&(oponente==0)] = coopera.trapaceia
    saida[(voce==0)&(oponente==0)] = trapaceia.trapaceia
    return(saida)
}

# Exemplo da função para tres jogos
retorno(c(0,0,0),c(1,1,0))


#Os tipos de agentes.

#Os agentes simples:
sempre.coopera   <- function(h) { return(TRUE) }
sempre.trapaceia <- function(h) { return(FALSE) }


# Imprevisivel
imprevisivel <- function(h) { return(1==rbinom(1,1,.5)) }

#tit4tat

tit4tat <- function(h) {
    if (length(h)==0) {
        return(TRUE)
    } else {
        return(rev(h)[1]==1)
    }
}


# tit4tat inicialmente agressivo
ag.tit4tat <- function(h) {
  if (length(h)==0) {
      return(FALSE)
  } else {
      return(rev(h)[1]==1)
  }
}

# tit4tat oportunista
op.tit4tat <- function(h) {
  if (length(h)==0) {
      return(TRUE)
  } else {
      return(((n<n.iteracoes-n.agentes)&(rev(h)[1]==1)))
  }
}


# fairbot
fairbot <- function(h) {
  if (length(h)==0) {
      return(TRUE)
  } else {
      return(mean(h,na.rm=T)>=.5)
  }
}

# fairbot inicialmente agressivo
ag.fairbot <- function(h) {
    if (length(h)==0) {
        return(FALSE)
    } else {
        return(mean(h,na.rm=T)>=.5)
    }
}

# Ditador covarde
ditador <- function(h) {
    if (length(h)==0) {
        return(TRUE)
    } else {
        return(mean(h,na.rm=T)<=.5)
    }
}

# Ditador Agressiveo
ag.ditador <- function(h) {
    if (length(h)==0) {
        return(FALSE)
    } else {
        return(mean(h,na.rm=T)<=.5)
    }
}

# Frenemy
frenemy <- function(h) {
    if (length(h)==0) {
        return(TRUE)
    } else {
        return(mean(rev(h)[1:2],na.rm=T)==1)
    }
}

ag.ditador(c())

ag.ditador(c(T,T,T,T,T,T))
ag.ditador(c(T,T,F,F,F))
ag.ditador(c(T,T,F,F,F,F,T,T,T))


#Parametros da simulação
n.iteracoes <- 500
n.agentes   <- 20

lista.agentes <- c("sempre.coopera", "sempre.trapaceia", "tit4tat" , "ag.tit4tat",
               "fairbot"    , "ag.fairbot"  , "ditador", "ag.ditador",
               "op.tit4tat" , "imprevisivel"  , "frenemy")

# Selecionando agentes para a simulação
agentes <- sort(sample(lista.agentes, n.agentes, replace=T))

# Matriz da historia de cada jogador
agentes.numero <- matrix(NA, ncol=n.agentes, nrow=n.iteracoes)

# Matriz de ações dos agentes
agentes.acoes.recebidas <- matrix(NA, ncol=n.agentes, nrow=n.iteracoes)
agentes.acoes.aplicadas <- matrix(NA, ncol=n.agentes, nrow=n.iteracoes)

# Pontuação historicas
agentes.pontos <- matrix(NA, ncol=n.agentes, nrow=n.iteracoes)

for (n in 1:n.iteracoes) {
  while (sum(is.na(agentes.numero[n,]))>0) {
  sempar <- 1:n.agentes
    while (length(sempar)>1) {
      i <- sempar[1]
      sempar <- sempar[sempar!=i]
      i.adversario <- sample(rep(sempar,2), 1)
      sempar <- sempar[sempar!=i.adversario]
      agentes.numero[n,i] <- i.adversario
      agentes.numero[n,i.adversario] <- i
    }
  }
  for (i in 1:n.agentes) {
    a.encarado <- agentes.numero[n,i]
    a.numeros <- agentes.numero[,i]
    a.acao <- agentes.acoes.recebidas[,i]
    h <- a.acao[(a.numeros==a.encarado)&(n>1:n.iteracoes)]
    response <- get(agentes[i])(h)
    agentes.acoes.aplicadas[n,i] <- response
    agentes.acoes.recebidas[n,a.encarado] <- response
  }
}

# Calculando os scores por rodada
placar.interacao <- retorno(agentes.acoes.aplicadas, agentes.acoes.recebidas)
placar.acumulado <- apply(placar.interacao, 2, cumsum)


jpeg("01.jpg")
cores <- rainbow(length(lista.agentes))
plot(x=c(1,n.iteracoes+25),y=c(min(placar.acumulado),max(placar.acumulado)),type="n",frame=F,
     main="Performance ao longo do tempo", xlab="Encontro", ylab="Pontuação")
for (i in 1:n.agentes) {
    lines(1:n.iteracoes, placar.acumulado[,i], col=cores[agentes[i]==lista.agentes], lwd=2)
}
legend("topleft",lty=1,legend=lista.agentes,col=cores,bty="n",lwd=2)
dev.off()



