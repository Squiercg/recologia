###http://recologia.com.br/2016/10/diagrama-de-voronoi/
x<-c(2,4,6,6)
y<-c(3,3,5,2)

jpeg("01.jpg")
plot(x,y,pch=19,xlim=c(0,10),ylim=c(0,10))
dev.off()

ponto<-2

##
jpeg("02.jpg")
plot(x,y,pch=19,xlim=c(0,10),ylim=c(0,10))
lines(x[c(1,ponto)],y[c(1,ponto)],lty=3)
dev.off()

##
jpeg("03.jpg")
plot(x,y,pch=19,xlim=c(0,10),ylim=c(0,10))
lines(x[c(1,ponto)],y[c(1,ponto)],lty=3)
points(mean(x[c(1,ponto)]),mean(y[c(1,ponto)]),pch=19,col="red")
dev.off()

##
jpeg("04.jpg")
plot(x,y,pch=19,xlim=c(0,10),ylim=c(0,10))
lines(x[c(1,ponto)],y[c(1,ponto)],lty=3)
points(mean(x[c(1,ponto)]),mean(y[c(1,ponto)]),pch=19,col="red")

b<- abs(y[ponto]-y[1])/abs(x[ponto]-x[1])
a<- -b*x[ponto]+y[ponto]
abline(a,b)

bp<- -(ifelse(is.infinite(1/b),2^64,1/b))
ap<- -mean(x[c(1,ponto)])*bp+mean(y[c(1,ponto)])
abline(ap,bp)
dev.off()

###
jpeg("05.jpg")
plot(x,y,pch=19,xlim=c(0,10),ylim=c(-3,10))

for(i in (1:length(x))[-ponto]){

    lines(x[c(i,ponto)],y[c(i,ponto)],lty=3)
    points(mean(x[c(i,ponto)]),mean(y[c(i,ponto)]),pch=19,col="red")

    b<- (y[ponto]-y[i])/(x[ponto]-x[i])
    a<- -b*x[ponto]+y[ponto]

    bp<- -(ifelse(is.infinite(1/b),2^64,1/b))
    ap<- -mean(x[c(i,ponto)])*bp+mean(y[c(i,ponto)])
    abline(ap,bp)
}
dev.off()



#install.packages("tripack")
library(tripack)

jpeg("06.jpg")
map<-voronoi.mosaic(x,y)
plot(x,y,pch=19,col="red",xlim=c(0,10),ylim=c(-3,10))
plot.voronoi(map,pch=19,add=TRUE)
dev.off()

x<-runif(50)
y<-runif(50)

jpeg("07.jpg")
map<-voronoi.mosaic(x,y)
plot(x,y,pch=19,col="red")
plot.voronoi(map,pch=19,add=TRUE)
dev.off()
