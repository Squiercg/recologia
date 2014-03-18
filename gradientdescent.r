computeCost<- function(X, y, theta) {
    m <- length(y)
    J <- 0
    J = (1/(2*m)) * sum((X%*%theta-y)^2)
    return(J)
}

gradientDescent<- function(X, y, theta, alpha, num_iters) {
    m = length(y)
    J_history = matrix(0,nrow=num_iters,ncol=3)

    for(iter in 1:num_iters) {

        h <- ((X %*% theta)-y)

        temp1 <- theta[1,] - alpha * (1/m) * sum( h * X[,1] )
        temp2 <- theta[2,] - alpha * (1/m) * sum( h * X[,2] )

        theta[1,] = temp1
        theta[2,] = temp2

        J_history[iter,1] = temp1
        J_history[iter,2] = temp2

        J_history[iter,3] = computeCost(X, y, theta);

    }

    return(J_history)
}

gradientDescentMulti<-function(X, y, theta, alpha, num_iters) {

    m = length(y)
    J_history = matrix(0,nrow=num_iters,ncol=1+nrow(theta))

    for(iter in 1:num_iters) {

        for(j in 1:length(theta)) {
            theta[j,]<-theta[j,] - alpha * (1/m) * sum( ((X %*% theta)-y) * X[,j] )
            J_history[iter,j] = theta[j,]
        }

        J_history[iter,nrow(theta)+1] = computeCost(X, y, theta);
    }

    return(J_history)
}

#gerando  dados
X<-runif(30,1,10)
y<-rnorm(30,5+2*X)

#figura 1
plot(y~X,main="Modelo 1+1*X",frame=F,xlim=c(0,10),ylim=c(0,30))
abline(a=1,b=1)
for(i in 1:length(X)) {
    points(c(X[i],X[i]),c(y[i],1+1*X[i]),type="l",lty=1,col="red",lwd=2)
}

#figura 2
plot(y~X,main="Modelo 5+2*X",frame=F,xlim=c(0,10),ylim=c(0,30))
abline(a=5,b=2)
for(i in 1:length(X)) {
    points(c(X[i],X[i]),c(y[i],5+2*X[i]),type="l",lty=1,col="blue",lwd=2)
}

#figura 3
par(mfrow=c(2,1))
plot(y~X,main="Dados sem normalização",frame=F)
abline(v=mean(X),h=mean(y),lty=2)
plot(scale(y)~scale(X),main="Dados normalizados",frame=F)
abline(v=mean(scale(X)),h=mean(scale(y)),lty=2)

#transformando em matrix
X<-matrix(c(rep(1,length(X)),scale(X)),ncol=2)
y<-matrix(scale(y),ncol=1)

#gerando theta inicial
theta<-matrix(c(-5,-5),ncol=1,nrow=2)
theta

#custo inicial
computeCost(X, y, theta)

#outros parametros
iterations = 1500
alpha = 0.01


#gradient descent

resultado<-gradientDescent(X, y, theta, alpha, iterations)

resultado
lm(y~X[,2])
#figura 4
plot(resultado[1:200,3],type="b")

#figura 5
plot(y~X[,2],frame=F)
abline(lm(y~X[,2]),col="red",lty=2)
abline(a=resultado[1500,1],b=resultado[1500,2],col="blue",lty=3)
legend("topleft",lty=c(2,3),col=c("red","blue"),legend=c("Regressão Linear","Gradient Descent"))

#figura 6
plot(resultado[,1],resultado[,2],type="b")

a<-seq(-2,2,by=0.1)
b<-seq(-2,3,by=0.1)
J<-matrix(0,nrow=length(a),ncol=length(b))

for(i in 1:length(a)) {
    for(j in 1:length(b)) {
        J[i,j]<--1*computeCost(X, y, matrix(c(a[i],b[j]),ncol=1,nrow=2))

    }
}

#figura 7
persp(a,b,J,theta=45,phi = 30,xlab="Intercepto",ylab="Inclinação",zlab="Menor custo")

#figura 8
contour(a,b,J)
points(resultado[1:300,1],resultado[1:300,2],type="b",cex=0.3)

##Regressão Multipla
X1<-runif(30,1,10)
X2<-runif(30,1,10)
y<-rnorm(30,5+2*X1+2*X2)

X<-matrix(c(rep(1,length(X1)),scale(X1),scale(X2)),ncol=3)
y<-matrix(scale(y),ncol=1)

theta<-matrix(c(0,0,0),ncol=1,nrow=3)
theta

computeCost(X, y, theta)

gradientDescentMulti(X, y, theta, alpha, 1500)
lm(y~X[,2]+X[,3])

X1<-runif(30,1,10)
X2<-runif(30,1,10)
y<-rnorm(30,5+2*X1+2*X2+3*X1*X2)

X<-matrix(c(rep(1,length(X1)),scale(X1),scale(X2),scale(X1)*scale(X2)),ncol=4)
y<-matrix(scale(y),ncol=1)

theta<-matrix(c(0,0,0,0),ncol=1,nrow=4)
theta

computeCost(X, y, theta)

gradientDescentMulti(X, y, theta, alpha, 1500)
lm(y~X[,2]*X[,3])









