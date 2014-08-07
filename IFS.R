matriz<-matrix(c(0,0,0,0.16,0,0,0.01,0.85,0.04,-0.04,0.85,0,1.6,0.85,0.2,-0.26,0.23,0.22,0,1.6,0.07,-0.15,0.28,0.26,0.24,0,0.44,0.07)
               ,byrow=T,nrow=4,ncol=7,dimnames = list(c("f1", "f2","f3","f4"),c("a", "b", "c","d","e","f","p")))
matriz


x<-0
y<-0
n<-3000
f<-matrix(0,ncol=4,nrow=2)
saida<-matrix(NA,ncol=2,nrow=n)

for(i in 1:n) {
    f[,1]<-matrix(matriz[1,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[1,5:6],2,1,byrow=T)
    f[,2]<-matrix(matriz[2,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[2,5:6],2,1,byrow=T)
    f[,3]<-matrix(matriz[3,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[3,5:6],2,1,byrow=T)
    f[,4]<-matrix(matriz[4,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[4,5:6],2,1,byrow=T)
    ponto<-sample(1:nrow(matriz),1,prob=matriz[,7])
    x<-f[1,ponto]
    y<-f[2,ponto]
    saida[i,]<-c(f[,ponto])
}


jpeg("01.jpg")
plot(saida)
dev.off()

jpeg("02.jpg")
plot(saida,pch=19,cex=0.5)
dev.off()
#############################################################
matriz<-matrix(c(0,0,0,0.25,0,-0.4,0.02,0.95,0.005,-0.005,0.93,-0.002,0.5,0.84,0.035,-0.2,0.16,0.04,-0.09,0.02,0.07,
                 -0.04,0.2,0.16,0.04,0.083,0.12,0.07),byrow=T,nrow=4,ncol=7,dimnames = list(c("f1", "f2","f3","f4"),
                                                                                c("a", "b", "c","d","e","f","p")))
matriz
x<-0
y<-0
n<-100000
f<-matrix(0,ncol=4,nrow=2)
saida<-matrix(NA,ncol=2,nrow=n)

for(i in 1:n) {
    f[,1]<-matrix(matriz[1,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[1,5:6],2,1,byrow=T)
    f[,2]<-matrix(matriz[2,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[2,5:6],2,1,byrow=T)
    f[,3]<-matrix(matriz[3,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[3,5:6],2,1,byrow=T)
    f[,4]<-matrix(matriz[4,1:4],2,2,byrow=T)%*%matrix(c(x,y),2,1,byrow=T)+matrix(matriz[4,5:6],2,1,byrow=T)
    ponto<-sample(1:nrow(matriz),1,prob=matriz[,7])
    x<-f[1,ponto]
    y<-f[2,ponto]
    saida[i,]<-c(f[,ponto])
}

jpeg("03.jpg")
plot(saida,pch=19,cex=0.4)
dev.off()

#####################################
IFS<-function(T,dist,cx,cy) {
    saida<-matrix(NA,ncol=2,nrow=T)
    x <- 0.0
    y <- 0.0
    for(t in 1:T) {
        r <- sample(1:length(dist),1,prob=dist)
        x0 <- cx[r,1]*x + cx[r,2]*y + cx[r,3]
        y0 <- cy[r,1]*x + cy[r,2]*y + cy[r,3]
        x <- x0
        y <- y0
        saida[t,]<-c(x, y)
    }
    return(saida)
}

########################
#Samambaia
dist<-c(0.01,0.85,0.07,0.07)
cx<-matrix(c(0.00,0.00,0.50,0.85,0.04,0.075,0.20,-0.26,0.400,-0.15,0.28,0.575),ncol=3,nrow=4,byrow=T)
cy<-matrix(c(0.00,0.16,0.00,-0.04,0.85,0.18,0.23,0.22,0.045,0.26,0.24,-0.086),ncol=3,nrow=4,byrow=T)

samambaia<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("04.jpg")
plot(samambaia,pch=19,cex=0.3,col="darkgreen",main="Samambaia")
dev.off()

#Samambaia Culcita
dist<-c(0.0200, 0.8400, 0.0700, 0.0700)

cx<-matrix(c( 0.000,  0.000,  0.500,
              0.850,  0.020,  0.075,
              0.090, -0.280,  0.455,
             -0.090,  0.280,  0.545),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c( 0.000,  0.250, -0.014,
             -0.020,  0.830,  0.110,
              0.300,  0.110, -0.090,
              0.300,  0.090, -0.080),ncol=3,nrow=length(dist),byrow=T)

Culcita<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("05.jpg")
plot(Culcita,pch=19,cex=0.3,col="darkgreen",main="Samambaia Culcita")
dev.off()

#Samambaia Cyclosorus
dist<-c(0.02, 0.84, 0.07, 0.07)

cx<-matrix(c(0.000,  0.000,  0.500,
             0.950,  0.005,  0.025,
             0.035, -0.200,  0.474,
            -0.040,  0.200,  0.528),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c( 0.000,  0.250, -0.040,
             -0.005,  0.930,  0.053,
              0.160,  0.040, -0.078,
              0.160,  0.040, -0.068),ncol=3,nrow=length(dist),byrow=T)

Cyclosorus<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("06.jpg")
plot(Cyclosorus,pch=19,cex=0.3,col="darkgreen",main="Samambaia Cyclosorus")
dev.off()


#Samambaia sedgewick
dist<-c(0.02,0.15,0.13,0.70)

cx<-matrix(c(0.000,0.000,0.500,
             -0.139,0.263,0.570,
             0.170,-0.215,0.408,
             0.781,0.034,0.1075),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.000,0.270,0.000,
             0.246,0.224,-0.036,
             0.222,0.176,0.0893,
             -0.032,0.739,0.270),ncol=3,nrow=length(dist),byrow=T)

Sedgewick<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("07.jpg")
plot(Sedgewick,pch=19,cex=0.3,col="darkgreen",main="Samambaia Sedgewick")
dev.off()

#arvore
dist<-c(0.1,0.1,0.2,0.2,0.2,0.2)

cx<-matrix(c(0.00,0.00,0.550,
            -0.05,0.00,0.525,
             0.46,-0.15,0.270,
             0.47,-0.15,0.265,
             0.43,0.28,0.285,
             0.42,0.26,0.290),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.00,0.60,0.000,
            -0.50,0.00,0.750,
             0.39,0.38,0.105,
             0.17,0.42,0.465,
            -0.25,0.45,0.625,
            -0.35,0.31,0.525),ncol=3,nrow=length(dist),byrow=T)

arvore<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("08.jpg")
plot(arvore,pch=19,col="darkgreen",cex=0.3,main="Árvore")
dev.off()

#Triângulo de sierpinski
dist<-c(0.33,0.33,0.34)
cx<-matrix(c(0.50,0.00,0.00,0.50,0.00,0.50,0.50,0.00,0.25),ncol=3,nrow=3,byrow=T)
cy<-matrix(c(0.00,0.50,0.00,0.00,0.50,0.00,0.00,0.50,0.433),ncol=3,nrow=3,byrow=T)

triangulo<-IFS(10000,dist=dist,cx=cx,cy=cy)

jpeg("09.jpg")
plot(triangulo,pch=19,cex=0.4,main="Triângulo de sierpinski")
dev.off()


#binary
dist<-c(0.3333,0.3333,0.3334)
cx<-matrix(c(0.5,0.0,-0.0063477,0.5,0.0,0.4936544,0.0,-0.5,0.9873085),ncol=3,nrow=3,byrow=T)
cy<-matrix(c(0.0,0.5,-0.0000003,0.0,0.5,-0.0000003,0.5,0.0,0.5063492),ncol=3,nrow=3,byrow=T)

binary<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("10.jpg")
plot(binary,pch=19,cex=0.3,main="binary")
dev.off()


#dragon
dist<-c(0.787473,  0.212527)

cx<-matrix(c( 0.824074,  0.281482, -0.1002660,
   0.088272,  0.520988,  0.5344000),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(-0.212346,  0.864198,  0.0951123,
  -0.463889, -0.377778,  1.0415240),ncol=3,nrow=length(dist),byrow=T)

dragon<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("11.jpg")
plot(dragon,pch=19,cex=0.3,main="dragon")
dev.off()


#Espinha de Peixe
dist<-c(0.0200,0.8400,0.0700,0.0700)

cx<-matrix(c(0.000,0.000,0.500,
             0.950,0.002,0.025,
             0.035,-0.110,0.478,
             -0.040,0.110,0.525),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.000,0.250,-0.040,
            -0.002,0.930,0.051,
             0.270,0.010,-0.135,
             0.270,0.010,-0.129),ncol=3,nrow=length(dist),byrow=T)

espinha<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("12.jpg")
plot(espinha,pch=19,cex=0.3,main="Espinha de Peixe")
dev.off()

#Chão
dist<-c(0.3333,0.3333,0.3334)

cx<-matrix(c(0.0,-0.5,0.3267634,
             0.5,0.0,0.2472109,
             0.0,0.5,0.6620804),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.5,0.0,0.0866182,
             0.0,0.5,0.5014877,
            -0.5,0.0,0.5810401),ncol=3,nrow=length(dist),byrow=T)

chao<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("13.jpg")
plot(chao,pch=19,cex=0.3,main="Chão")
dev.off()

#koch
dist<-c(0.151515  0.253788  0.253788  0.151515  0.189394)

cx<-matrix(c(0.307692,-0.000000,0.7580704,
  0.192308,-0.205882,0.3349620,
  0.192308,0.205882,0.4707040,
  0.307692,-0.000000,-0.0674990,
  0.307692,-0.000000,0.3453822),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.000000,0.294118,0.1604278,
  0.653846,0.088235,0.2709686,
  -0.653846,0.088235,0.9231744,
  0.000000,0.294118,0.1604278,
  0.000000,-0.294118,0.2941176),ncol=3,nrow=length(dist),byrow=T)

koch<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("14.jpg")
plot(koch,pch=19,cex=0.3,main="koch")
dev.off()

#Espiral
dist<-c(0.895652,0.052174,0.052174)

cx<-matrix(c(0.787879,-0.424242,0.2819252,
            -0.121212,0.257576,-0.1115594,
             0.181818,-0.136364,1.0177017),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.242424,0.859848,0.0195945,
             0.151515,0.053030,0.0619661,
             0.090909,0.181818,0.1113490),ncol=3,nrow=length(dist),byrow=T)

espiral<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("15.jpg")
plot(espiral,pch=19,cex=0.3,main="Espiral")
dev.off()

#swirl pattern
dist<-c(0.9126750,0.0873250)

cx<-matrix(c(0.745455,-0.459091,0.2733004,
            -0.424242,-0.065152,1.0930777),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(0.406061,0.887121,-0.1339233,
            -0.175758,-0.218182,0.7620266),ncol=3,nrow=length(dist),byrow=T)

swirl<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("16.jpg")
plot(swirl,pch=19,cex=0.3,main="Swirl Pattern")
dev.off()

#zig-zag
dist<-c(0.888128,0.111872)

cx<-matrix(c(-0.632407,-0.614815,1.2002857,
             -0.036111,0.444444,0.7251636),ncol=3,nrow=length(dist),byrow=T)

cy<-matrix(c(-0.545370,0.659259,0.4009171,
              0.210185,0.037037,0.7279627),ncol=3,nrow=length(dist),byrow=T)

zigzag<-IFS(100000,dist=dist,cx=cx,cy=cy)

jpeg("17.jpg")
plot(zigzag,pch=19,cex=0.3,main="Zig Zag")
dev.off()
