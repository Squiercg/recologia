###################################################
### Casos de equilibrio para duas populações 
###################################################

### Coexistencia estavel

layout(matrix(c(1:4),ncol=2,byrow=T))

a <- matrix(c(0.01,0.005,0.005,0.01),ncol=2,byrow=TRUE)

plot( (a[2,2]-a[1,2])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]),
     (a[1,1]-a[2,1])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]), cex=2, pch=1, col=1, axes=FALSE,
     ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
     ylab=expression("N"[2]), xlab=expression("N"[1]), pty='s')

curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], add=TRUE, lty=2)
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
box()
arrows(c(25,25,25), c(25,25,25), c(25,50,50), c(50,25,50),
       col=1, length=.12, lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(150,150,150), c(150,150,150), c(150,120, 120), c(120,150,120),
      col=1, length=.12,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(10, 10, 10), c(125,125,125), c(10,30,30), c(105,125,105),
       length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))
arrows(c(125,125,125), c(10, 10,10), c(125,105,105), c(30,10,30), 
       length=0.12, col=1,  lwd=c(1,1,2), lty=c(2,1,1))

### Espécie 2 pode invadir, a espécie 1 não

a <- matrix(c(0.01, 0.02,0.005, 0.01), ncol=2, byrow=TRUE)

curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], lwd=1, lty=2,  axes=FALSE,
      ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
      ylab=expression("N"[2]), xlab=expression("N"[1]))

curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]), lwd=1,c(0, expression(1/alpha[22]),expression(1/alpha[12])))
box()
xs <- c(1, 1, .4, .4) * a[1,1]^-1
ys <- c(.2, .4, .4, .7)*a[2,2]^-1

arrows(xs[1:3], ys[1:3], xs[2:4], ys[2:4],lty=2:1, lwd=c(1,1), length=0.12)
points(0,1/a[2,2], cex=2)

### Espécie 1 pode invadir, espécie 2 não

a <- matrix(c(0.01, 0.005,0.02,0.01), ncol=2, byrow=TRUE)
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], lwd=1, lty=2,  axes=FALSE,
      ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
      ylab=expression("N"[2]), xlab=expression("N"[1]))
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))

ys <- c(1, 1, .4, .4) * a[1,1]^-1
xs <- c(.2, .4, .4, .7)*a[2,2]^-1

arrows(xs[1:3], ys[1:3], xs[2:4], ys[2:4],lty=1:2, lwd=c(1,1), length=0.12)
points(1/a[1,1],0, cex=2)

### Nenhuma especie pode invadir, com equilibrio estavel

a <- matrix(c(  0.01, 0.02,0.02, 0.01), ncol=2, byrow=TRUE)
curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], lty=2,  axes=FALSE,
      ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
      ylab=expression("N"[2]), xlab=expression("N"[1]))
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)

axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))

pxys <- matrix(c(.13,.1,.25,.1,
                 .1,.13,.1,.25,
                 .115,.115,.25,.25,
                 .7,.8,.4,.8,
                 .8,.7,.8,.4,
                 .75,.75,.4,.4,
                 .25,.4,.1, .6,
                 .4,.25,.6, .1),
              nc=4, byrow=T)
xys <- pxys
xys[,1:2] <-  a[1,1]^-1 * pxys[,1:2]
xys[,3:4] <-  pxys[,3:4] * a[2,2]^-1
arrows(xys[,1], xys[,2], xys[,3], xys[,4],lty=c(1,2,3,1,2,3,2,1), lwd=c(1), length=0.12)
n2star <- (a[1,1] - a[2,1])/(a[1,1]*a[2,2] - a[1,2]*a[2,1])
n1star <- (a[2,2] - a[1,2])/(a[1,1]*a[2,2] - a[1,2]*a[2,1])
points(n1star,n2star, cex=2)

#Interações ao acaso
par(mfrow=c(3,3))

for(i in c(1:9)) {
a <- matrix(runif(4,0,2),ncol=2,byrow=TRUE)

plot( (a[2,2]-a[1,2])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]),
     (a[1,1]-a[2,1])/(a[2,2]*a[1,1] - a[1,2]*a[2,1]), cex=2, pch=1, col=1, axes=FALSE,
     ylim=c(0, max(1/a[2,2], 1/a[1,2])), xlim=c(0, max(1/a[1,1], 1/a[2,1])),
     ylab=expression("N"[2]), xlab=expression("N"[1]), pty='s')

curve(1/a[2,2] - (a[2,1]/a[2,2])*x, 0, 1/a[2,1], add=TRUE, lty=2)
curve(1/a[1,2] - (a[1,1]/a[1,2])*x, 0, 1/a[1,1], add=T)
axis(1, c(0, 1/a[1,1], 1/a[2,1]),c(0, expression(1/alpha[11]),expression(1/alpha[21])))
axis(2, c(0, 1/a[2,2], 1/a[1,2]),c(0, expression(1/alpha[22]),expression(1/alpha[12])))
legend("topright",legend=paste(c("1,1=","1,2=","2,1=","2,2="),round(a,digits=2)),bty="n")
}

