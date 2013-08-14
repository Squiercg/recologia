#State-Space
plot(0, 0, type = "p", ylim = c(0, 200), xlim = c(0,200), ylab = expression("N"[2]),xlab=expression("N"[1]))

#alphas
a <- matrix(c(0.01, 0.005, 0.005, 0.01), ncol = 2, byrow = TRUE)
a

N2iso <- expression(1/a[2, 2] - (a[2, 1]/a[2, 2]) * N1)

N2iso
class(N2iso)

eval(2^2)

espressao<-expression(2^2)

eval(espressao)

espressao<-expression(x^2)

eval(espressao)

ls()

x<-2

ls()

eval(espressao)

#Isocline da Especie 1
N1 <- 0:200
plot(N1, eval(N2iso), type = "l", ylim = c(0, 200), xlim = c(0,200), ylab = expression("N"[2]))
arrows(x0 = 90, y0 = 150, x1 = 90, y1 = 80, length = 0.1)
arrows(x0 = 75, y0 = 0, x1 = 75, y1 = 50, length = 0.1)


#Isocline da Especie 1 usando a função curve
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], axes=FALSE, lty=2,
      ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
      ylab=expression("N"[2]), xlab=expression("N"[1]), pty="s" )

axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
arrows(c(75, 175), c(150,150), lty=2,c(75,175), c(80,45), length=0.12)
arrows(c(25, 125), c(5, 5), lty=2,c(25,125), c(55, 20), length=0.12)

### Isocline da especie 2
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1],axes=FALSE,ylim=c(0, max(1/a[2,2], 1/a[1,2])),
      xlim=c(0, max(1/a[1,1], 1/a[2,1])),ylab=expression("N"[2]), xlab=expression("N"[1]),pty='s' )
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
arrows(c(150,150), c(75, 175),c(80,45), c(75,175), length=0.12)
arrows( c(5, 5), c(25, 125),c(55, 20), c(25,125), length=0.12)


### Ambas isoclines no mesmo grafico

plot((a[2,2]-a[1,2])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]),(a[1,1]-a[2,1])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]), cex=2,
     pch=1, col=1, axes=FALSE,ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
     ylab=expression("N"[2]), xlab=expression("N"[1]), pty='s')
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], add=TRUE, lty=2)
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
arrows(c(25,25,25), c(25,25,25), c(25,50,50), c(50,25,50),col=1, length=.12, lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(150,150,150), c(150,150,150), c(150,120, 120), c(120,150,120),col=1, length=.12,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(10, 10, 10), c(125,125,125), c(10,30,30), c(105,125,105),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(125,125,125), c(10, 10,10), c(125,105,105), c(30,10,30),length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))

