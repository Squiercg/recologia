#Função Sigmoid
sigmoid<- function(z) {
    return(1/(1+exp(-z)))
}

#Função de custo
costFunction<- function(theta, X, y) {
    m <- length(y)
    J <- 0
    grad <- matrix(0,nrow=length(theta)+1,ncol=1)
    h <- sigmoid(X %*% theta)
    costPos <- t(-y) %*% log(h)
    costNeg <- (1 - t(y)) %*% log(1 - h)
    J = (1/m) * (costPos - costNeg)
    grad = (1/m) * (t(X) %*% (h - y))
    return(list(J=c(J),grad=grad))
}

#Função de custo regularizado
costFunctionReg <-function(theta, X, y, lambda) {
    m <- length(y)
    J <- 0
    grad <- matrix(0,nrow=length(theta),ncol=1)
    saida<-costFunction(theta, X, y);
    theta<-saida$grad
    J<-saida$J
    J = J + ((lambda / (2*length(y))) * (t(theta)%*%theta))
    grad[1,1] = theta[1,1]
    grad[-1,1] = grad[-1,1] + ((lambda /length(y)) * theta[-1,1]) 
    return(list(J=c(J),grad=grad))
}

#Função para mapear a entrada em mais graus
mapFeature<- function(X1,X2,degree = 6) {
    out <-matrix(0,ncol=1,nrow=length(X1))
    out[,1]<-rep(1,length(X1))
     for(i in 1:degree) {
        for(j in 0:i) {
            out = cbind(out,(X1^(i-j))*(X2^j))
        }
    }
    return(out)
}

####################
exemplo2<-read.table("ex2data2.txt",sep=",",stringsAsFactors =FALSE)
exemplo2[,1]<-as.numeric(exemplo2[,1])
exemplo2[,2]<-as.numeric(exemplo2[,2])
colnames(exemplo2)<-c("QA1","QA2","Status")

#
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","green"),pch=19,xlab="Tesde de microship 1",
     ylab="Teste de microship 2",frame=F)
legend("topright",c("Aceito","Rejeitado"),pch=19,col=c("green","red"),bty="n")

#Parametros de entrada
X<-as.matrix(cbind(1,exemplo2[,1:2]))
theta<-matrix(0,nrow=ncol(exemplo2[,1:2])+1,ncol=1)
y<-exemplo2[,3]

ajuste<-optim(c(0,0,0),function(theta) {costFunction(theta,X,y)$J},method="Nelder-Mead",control = list(trace=T))
ajuste


#Figura 2
plot_x<-c(min(X[,2])-2,max(X[,2])+2)
plot_y<-(-1/ajuste$par[3])*(ajuste$par[2]*plot_x + ajuste$par[1])
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
lines(plot_x,plot_y,lty=3,lwd=4,col="black")
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")


#################################
X<-mapFeature(exemplo2[,1],exemplo2[,2])
theta<-matrix(0,nrow=ncol(X),ncol=1)
y<-exemplo2[,3]

ajuste<-optim(rep(0,ncol(X)),function(theta) {costFunction(theta,X,y)$J},method="Nelder-Mead",control = list(trace=T))
ajuste$par

#Figura 3
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
u<-seq(-1, 1.5,length= 50);
v<-seq(-1, 1.5,length= 50);
z<-matrix(0,ncol=length(u),nrow=length(v))
for(i in 1:length(u)) {
    for(j in 1:length(v)) {
        z[i,j]<-mapFeature(u[i], v[j])%*%ajuste$par
    }
}
z = t(z)
contour(u,v,z,1,drawlabels=F,add=T,lwd=3,lty=2)
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")


###############################
lambda <- 1
ajuste<-optim(rep(0,ncol(X)),method="L-BFGS-B",control = list(trace=T),
              fn =function(theta) {costFunctionReg(theta,X,y,lambda)$J},
              gr =function(theta) {costFunctionReg(theta,X,y,lambda)$grad})
ajuste$par

#Figura 4
#jpeg("04.jpg")
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
u<-seq(-1, 1.5,length= 50);
v<-seq(-1, 1.5,length= 50);
z<-matrix(0,ncol=length(u),nrow=length(v))
for(i in 1:length(u)) {
    for(j in 1:length(v)) {
        z[i,j]<-mapFeature(u[i], v[j])%*%ajuste$par
    }
}
z = t(z)
contour(u,v,z,1,drawlabels=F,add=T,lwd=3,lty=2)
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")
#dev.off()
###############################
lambda <- 0.01
ajuste<-optim(rep(0,ncol(X)),method="L-BFGS-B",control = list(trace=T),
              fn =function(theta) {costFunctionReg(theta,X,y,lambda)$J},
              gr =function(theta) {costFunctionReg(theta,X,y,lambda)$grad})
ajuste$par

#Figura 5
#jpeg("05.jpg")
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
u<-seq(-1, 1.5,length= 50);
v<-seq(-1, 1.5,length= 50);
z<-matrix(0,ncol=length(u),nrow=length(v))
for(i in 1:length(u)) {
    for(j in 1:length(v)) {
        z[i,j]<-mapFeature(u[i], v[j])%*%ajuste$par
    }
}
z = t(z)
contour(u,v,z,1,drawlabels=F,add=T,lwd=3,lty=2)
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")
#dev.off()

###############################

lambda <- 1000
ajuste<-optim(rep(0,ncol(X)),method="L-BFGS-B",control = list(trace=T),
              fn =function(theta) {costFunctionReg(theta,X,y,lambda)$J},
              gr =function(theta) {costFunctionReg(theta,X,y,lambda)$grad})
ajuste$par

#Figura 6
#jpeg("06.jpg")
plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
u<-seq(-1, 1.5,length= 100);
v<-seq(-1, 1.5,length= 100);
z<-matrix(0,ncol=length(u),nrow=length(v))
for(i in 1:length(u)) {
    for(j in 1:length(v)) {
        z[i,j]<-mapFeature(u[i], v[j])%*%ajuste$par
    }
}
z = t(z)
contour(u,v,z,1,drawlabels=F,add=T,lwd=3,lty=2)
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")
#dev.off()
