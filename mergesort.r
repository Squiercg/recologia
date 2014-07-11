############################################
#
############################################

merge<-function(vetor,tamanho) {
  meio<-tamanho%/%2
  copia<-rep(NA,tamanho)
  i <- 1
  j <- meio+1
  k <- 1
  while (i <= meio && j <= tamanho) {
    if (vetor[i] < vetor[j]) {
      copia[k] <- vetor[i]
      i<-i+1
    } else {
      copia[k] <- vetor[j]
      j<-j+1
    }
    k<-k+1
  }
  if (i > meio) {
    while (j <= tamanho) {
      copia[k] = vetor[j]
      j<-j+1
      k<-k+1
    }
  } else {
    while (i <= meio) {
      copia[k] <- vetor[i]
      i<-i+1
      k<-k+1
    }
  }
  return(copia)
}

########################################################
#
#########################################################
mergeSortR<-function(vetor,tamanho) {
  if (tamanho > 1) {
      meio <- tamanho %/% 2
      inicio<-mergeSortR(vetor[1:meio], meio)
      fim<-mergeSortR(vetor[(meio+1):tamanho], tamanho-meio)
      saida<-merge(c(inicio,fim), tamanho)
      return(saida)
  } else {
      return(vetor)
  }
}

mergeSortR(sample(1:1000),1000)
