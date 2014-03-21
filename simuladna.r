sample(c("Cara","Coroa"),30,rep=TRUE,prob=c(0.5,0.5))

sample(c("A","G","C","T"),30,rep=TRUE,prob=c(0.25,0.25,0.25,0.25))



markov1 <- function(x,P,n) {
    seq <- x
    for(k in 1:(n-1)){
        seq[k+1] <- sample(x, 1, replace=TRUE, P[seq[k],])
    }
    return(seq)
}

P <- matrix(c(1/6,5/6,0.5,0.5), 2, 2, byrow=TRUE)

rownames(P)<-c(1,2)
colnames(P)<-c(1,2)
x<-c(1,2)

markov1(x,P,30)

markov2 <- function(StateSpace,P,pi0,n) {
    seq <- character(n)
    seq[1] <- sample(StateSpace, 1, replace=TRUE, pi0)
    for(k in 1:(n-1)) {
        seq[k+1] <- sample(StateSpace, 1, replace=TRUE, P[seq[k],])
    }
    return(seq)
}

P <- matrix(c(
1/6,5/6,0,0,
1/8,2/4,1/4,1/8,
0,2/6,3/6,1/6,
0,1/6,3/6,2/6),4,4,byrow=TRUE)

rownames(P) <- colnames(P) <- StateSpace <- c("a","c","g","t")

pi0 <- c(1/4,1/4,1/4,1/4)

x <- markov2(StateSpace,P,pi0,1000)

head(x)
table(x)


P <- matrix(c(.01,.01,.01,.97,
.01,.01,.01,.97,
.01,.01,.01,.97,
.01,.28,.01,0.70),4,4,byrow=T)
rownames(P) <- colnames(P) <- StateSpace <- c("a","c","g","t")
x <- markov2(StateSpace,P,pi0,30000)

#install.packages("seqinr")
library(seqinr)
table(getTrans(x))

nr <- count(x,2)
nr

A <- matrix(NA,4,4)
rownames(A) <- colnames(A) <- c("a","c","g","t")
A[1,1]<-nr["aa"]; A[1,2]<-nr["ag"]; A[1,3]<-nr["ac"]; A[1,4]<-nr["at"]
A[2,1]<-nr["ga"]; A[2,2]<-nr["gg"]; A[2,3]<-nr["gc"]; A[2,4]<-nr["gt"]
A[3,1]<-nr["ca"]; A[3,2]<-nr["cg"]; A[3,3]<-nr["cc"]; A[3,4]<-nr["ct"]
A[4,1]<-nr["ta"]; A[4,2]<-nr["tg"]; A[4,3]<-nr["tc"]; A[4,4]<-nr["tt"]

A

rowsumA <- apply(A, 1, sum)
Phat <- sweep(A, 1, rowsumA, FUN="/")
rownames(Phat) <- colnames(Phat) <- c("a","g","c","t")

P
round(Phat,3)
