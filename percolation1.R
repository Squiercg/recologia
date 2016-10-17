###http://recologia.com.br/2016/10/o-problema-da-percolacao/
#####################################
## Problema da Percolação
#####################################
set.seed(123)

p<-0.5
mapa<-matrix(sample(x=c(0,1),size=100,replace=T,prob=c(p,1-p)),ncol=10,nrow=10)
mapa

jpeg("01.jpg")
image(t(mapa)[,ncol(mapa):1],col=c("brown","white"))
dev.off()

jpeg("02.jpg")
image(t(mapa)[,ncol(mapa):1],col=c("brown","blue"))
dev.off()

full<-matrix(0,ncol=ncol(mapa),nrow=nrow(mapa))

##Percolação Vertical
for (j in 1:ncol(mapa)) {
    full[1,j] <- mapa[1,j]
}
for (i in 2:nrow(mapa)) {
    for (j in 1:ncol(mapa)) {
        full[i,j] <- mapa[i,j]==1 & full[i-1,j]==1
    }
}

jpeg("03.jpg")
par(mfrow=c(1,2))
image(t(mapa)[,ncol(mapa):1],col=c("brown","blue"))
saida<-full
saida[!mapa==full]<-2
image(t(saida)[,ncol(mapa):1],col=c("brown","blue","white"))
dev.off()

###
imagem<-1

flow<-function(open,full,i,j) {
    ##casos bases
    ##Não posso olhar fora do tamanho das linhas
    if (i<1 || i>ncol(open)){
        return(full)
    }
    ##Nem fora do tamanho das colunas
    if (j<1 || j>ncol(open)){
        return(full)
    }
    ##Se o local esta fechado, acabou a percolação
    if (open[i,j]==0){
        return(full)
    }
    ##Se eu ja mexi nessa celula, não precimo mais olhar
    if (full[i,j]==1){
        return(full)
    } ## Ja marcado como cheio

    ##Se eu não estou em nenhum dos casos bases, ou seja, o local não esta bloqueado
    ##e eu ainda nao observei, quer dizer que tenho que preencher com 1 aqui, percolar
    full[i,j] <- 1;

    ##saida<-full
    ##saida[!mapa==full]<-2
    ##png(paste(sprintf("%03d", imagem),".png",sep=""))  #Descomente essa linha para salvar a i  
    ##image(t(saida)[,ncol(mapa):1],col=c("brown","blue","white"))
    ##dev.off()
    ##imagem<<-imagem+1
    ##Sys.sleep(1)

    
    ##Agora eu olho em todas as direções, se eu percolo
    ##Abaixo
    full<-flow(open, full, i+1, j);
    ##Direita
    full<-flow(open, full, i, j+1);
    ##Esquerda
    full<-flow(open, full, i, j-1);
    ##Acima
    full<-flow(open, full, i-1, j);
}


imagem<-1
full<-matrix(0,ncol=ncol(mapa),nrow=nrow(mapa))
for (j in 1:ncol(mapa)) {
    full<-flow(mapa, full, 1, j)
}

jpeg("04.jpg")
par(mfrow=c(1,2))
image(t(mapa)[,ncol(mapa):1],col=c("brown","blue"))
saida<-full
saida[!mapa==full]<-2
image(t(saida)[,ncol(mapa):1],col=c("brown","blue","white"))
dev.off()
