exemplo1<-read.table("ex2data1.txt",sep=",",stringsAsFactors =FALSE)
exemplo1[,1]<-as.numeric(exemplo1[,1])
exemplo1[,2]<-as.numeric(exemplo1[,2])
colnames(exemplo1)<-c("Exame1","Exame2","Status")

#Figura 1
plot(exemplo1[,1],exemplo1[,2],col=ifelse(exemplo1$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F)
legend("topright",c("Adminitido","Reprovado"),pch=19,col=c("blue","red"),bty="n")

#Função Sigmoid
sigmoid<- function(z) {
    return(1/(1+exp(-z)))
}

sigmoid(0)

#Figura 2
plot(seq(-6,6,0.01),sigmoid(seq(-6,6,0.01)),type="l")

#Parametros de entrada
X<-as.matrix(cbind(1,exemplo1[,1:2]))
theta<-matrix(0,nrow=ncol(exemplo1[,1:2])+1,ncol=1)
y<-exemplo1[,3]

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
    return(c(J))
}
costFunction(theta,X,y)

#Ajuste da regressão
ajuste<-optim(c(0,0,0),function(theta) {costFunction(theta,X,y)},method="Nelder-Mead",control = list(trace=T))
ajuste
costFunction(matrix(ajuste$par,nrow=ncol(exemplo1[,1:2])+1,ncol=1),X,y)

#Previsões
Nota_Exame1<-80
Nota_Exame2<-80
sigmoid(ajuste$par[1]+ajuste$par[2]*Nota_Exame1+ajuste$par[3]*Nota_Exame2)

sigmoid(ajuste$par[1]+ajuste$par[2]*90+ajuste$par[3]*40)
sigmoid(ajuste$par[1]+ajuste$par[2]*70+ajuste$par[3]*50)
sigmoid(ajuste$par[1]+ajuste$par[2]*50+ajuste$par[3]*50)

#Figura 3
plot_x<-c(min(X[,2])-2,max(X[,2])+2)
plot_y<-(-1/ajuste$par[3])*(ajuste$par[2]*plot_x + ajuste$par[1])
plot(exemplo1[,1],exemplo1[,2],col=ifelse(exemplo1$Status==0,"red","blue"),pch=19,xlab="Exame 1",ylab="Exame 2",frame=F,
     ylim=c(30,110))
lines(plot_x,plot_y,lty=3,lwd=4,col="black")
legend("topright",c("Adminitido","Reprovado","Margem de decisão"),pch=c(19,19,NA),lty=c(0,0,3),lwd=4,
       col=c("blue","red","black"),bty="n")

ajuste$par
glm(Status~Exame1+Exame2,data=exemplo1,family="binomial")
##############################

exemplo2<-read.table("ex2data2.txt",sep=",",stringsAsFactors =FALSE)
exemplo2[,1]<-as.numeric(exemplo2[,1])
exemplo2[,2]<-as.numeric(exemplo2[,2])
colnames(exemplo2)<-c("QA1","QA2","Status")

plot(exemplo2[,1],exemplo2[,2],col=ifelse(exemplo2$Status==0,"red","green"),pch=19,xlab="Tesde de microship 1",
     ylab="Teste de microship 2",frame=F)
legend("topright",c("Aceito","Rejeitado"),pch=19,col=c("green","red"),bty="n")

