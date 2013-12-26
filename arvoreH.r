desenheH<-function(x,y,tamanho) {
    #Calcule as coordenadas das 4 pontas do H
    x0 <- x - tamanho/2
    x1 <- x + tamanho/2
    y0 <- y - tamanho/2
    y1 <- y + tamanho/2

    #Desenhe 3 linhas que formam o H
    points(c(x0, x0),c(y0, y1),type="l",lty=1)    # Linha Esquerda
    points(c(x1, x1),c(y0, y1),type="l",lty=1)    # Linha Direita
    points(c(x0,  x1),c(y,  y),type="l",lty=1)    # Linha do meio
}

desenhe<-function(n,x,y,tamanho) {

    if (n == 0) {
        return("Fim da Recursão")
    } else {

        desenheH(x, y, tamanho)
        #Sys.sleep(0.5)

        #Calcule as coordenadas das 4 pontas do H
        x0 = x - tamanho/2
        x1 = x + tamanho/2
        y0 = y - tamanho/2
        y1 = y + tamanho/2

        #Desenhe, recursivamente, 4 arvores H com metade do tamanho e de ordem n-1
        desenhe(n-1, x0, y0, tamanho/2)    #// Árvore da esquerda de baixo
        desenhe(n-1, x0, y1, tamanho/2)    #// Árvore da esquerda de cima
        desenhe(n-1, x1, y0, tamanho/2)    #// Árvore da direita de baixo
        desenhe(n-1, x1, y1, tamanho/2)    #// Árvore da direita de cima
    }
}


x<-5
y<-5
tamanho<-2

jpeg("01.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(1, x, y, tamanho);
dev.off()

jpeg("02.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(2, x, y, tamanho);
dev.off()

jpeg("03.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(3, x, y, tamanho);
dev.off()

jpeg("04.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(4, x, y, tamanho);
dev.off()

jpeg("05.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(5, x, y, tamanho);
dev.off()

jpeg("06.jpg")
plot(x,y,type="n",xlim=c(x-1.5*tamanho,x+1.5*tamanho),ylim=c(y-1.5*tamanho,y+1.5*tamanho),frame=F)
desenhe(6, x, y, tamanho);
dev.off()


