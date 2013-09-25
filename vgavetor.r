#install.packages("scatterplot3d")
library(scatterplot3d)
?scatterplot3d
x=0
y=0
z=0

R3<-scatterplot3d(x,y,z,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),zlim=c(-1.5,1.5),type="n",main="",box=T)


#Plano YZ
x0 <- -1.5
xyz1 <- R3$xyz.convert(rep(x0, 7), rep( -1.5, 7), seq(-1.5, 1.5, by=0.5))
xyz2 <- R3$xyz.convert(rep(x0, 7), rep(1.5, 7), seq(-1.5, 1.5, by=0.5))
segments(xyz1$x, xyz1$y, xyz2$x, xyz2$y, lty="dotted",col="gray")

xyz1 <- R3$xyz.convert(rep(x0, 7), seq(-1.5, 1.5, by=0.5), rep(-1.5, 7))
xyz2 <- R3$xyz.convert(rep(x0, 7), seq(-1.5, 1.5, by=0.5), rep(1.5, 7))
segments(xyz1$x, xyz1$y, xyz2$x, xyz2$y, lty="dotted",col="gray")


#Plano XZ
y0 <- 1.5
xyz1 <- R3$xyz.convert(rep(-1.5, 7),  rep(y0, 7),seq(-1.5, 1.5, by=0.5))
xyz2 <- R3$xyz.convert(rep( 1.5, 7),  rep(y0, 7),seq(-1.5, 1.5, by=0.5))
segments(xyz1$x, xyz1$y, xyz2$x, xyz2$y, lty="dotted",col="gray")

xyz1 <- R3$xyz.convert(seq(-1.5, 1.5, by=0.5), rep(y0, 7), rep(-1.5, 7))
xyz2 <- R3$xyz.convert(seq(-1.5, 1.5, by=0.5), rep(y0, 7), rep( 1.5, 7))
segments(xyz1$x, xyz1$y, xyz2$x, xyz2$y, lty="dotted",col="gray")


arrows(x0=R3$xyz.convert(x=0,y=0,z=0)$x,
       y0=R3$xyz.convert(x=0,y=0,z=0)$y,
       x1=R3$xyz.convert(x=1,y=1,z=1)$x,
       y1=R3$xyz.convert(x=1,y=1,z=1)$y,
       length=0.1,lwd=2,col="red")

R3$plane3d(Intercept=c(0,0,0),lty=2,col="blue",lwd=0.5)

vga.vetor<-function(vetor,origem=c(0,0,0),...) {
    arrows(x0=R3$xyz.convert(x=origem[1],y=origem[2],z=origem[3])$x,
           y0=R3$xyz.convert(x=origem[1],y=origem[2],z=origem[3])$y,
           x1=R3$xyz.convert(x=origem[1]+vetor[1],y=origem[2]+vetor[2],origem[3]+vetor[3])$x,
           y1=R3$xyz.convert(x=origem[1]+vetor[1],y=origem[2]+vetor[2],origem[3]+vetor[3])$y,
           ...)
    }

vga.reta<-function(vetor,origem=c(0,0,0),...) {
    xyz1 <- R3$xyz.convert(origem[1],origem[2],origem[3])
    xyz2 <- R3$xyz.convert(origem[1]+vetor[1],origem[2]+vetor[2],origem[3]+vetor[3])
    segments(xyz1$x, xyz1$y, xyz2$x, xyz2$y,...)
    }



R3<-scatterplot3d(0,0,0,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),zlim=c(-1.5,1.5),type="n",main="")
R3$plane3d(Intercept=c(0,0,0),col="gray",lty=3)

vga.vetor(c(1,1,1),length=0.1)
vga.vetor(c(1,0,0),length=0.1,col="blue",lty=3)
vga.vetor(c(0,1,0),length=0.1,col="blue",lty=3)
vga.vetor(c(0,0,1),length=0.1,col="blue",lty=3)

R3<-scatterplot3d(x,y,z,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),zlim=c(-1.5,1.5),type="n",main="")
R3$plane3d(Intercept=c(0,0,0),col="gray",lty=3)
vga.vetor(c(1,0,0),length=0.1,col="blue",lty=3)
vga.vetor(c(0,1,0),c(1,0,0),length=0.1,col="blue",lty=3)
vga.vetor(c(1,1,0),length=0.1,col="red",lty=2)


R3<-scatterplot3d(x,y,z,xlim=c(-2,2),ylim=c(-2,2),zlim=c(-2,2),type="n",main="")
R3$plane3d(Intercept=c(0,0,0),col="gray",lty=3)
vga.reta(c(0,0,4),c(1,1,-2),col="gray",lty=3)
vga.vetor(c(1,1,1),length=0.1)
vga.vetor(c(1,0,0),length=0.1,col="blue",lty=3)
vga.vetor(c(0,1,0),length=0.1,col="blue",lty=3)
vga.vetor(c(0,0,1),c(1,1,0),length=0.1,col="blue",lty=3)

