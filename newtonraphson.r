newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
    x <- x0
    fx <- ftn(x)
    iter <- 0

    while ((abs(fx[1]) > tol) && (iter < max.iter)) {
        x <- x - fx[1]/fx[2]
        fx <- ftn(x)
        iter <- iter + 1
        cat("At iteration", iter, "value of x is:", x, "\n")
    }
    # output depends
    if (abs(fx[1]) > tol) {
        cat("Algorithm failed to converge\n")
        return(NULL)
    } else {
        cat("Algorithm converged\n")
        return(x)
    }
}

ftn4 <- function(x) {
    fx <- log(x) - exp(-x)
    dfx <- 1/x + exp(-x)
    return(c(fx, dfx))
}

newtonraphson(ftn4, 2, 1e-06)


curve(log(x) - exp(-x),0,3,frame=F)
points(1.3098,ftn4(1.3098)[1],cex=2)
abline(v=1.3098,h=ftn4(1.3098)[1],lty=2)


par(mfrow=c(2,1))
curve(log(x) - exp(-x),0,3,frame=F,main="Função")
abline(v=1.3098,h=ftn4(1.3098)[1],lty=2)
curve(1/x + exp(-x),0,3,frame=F,main="Primeira derivada")

ftn4 <- function(x) {
    fx <- x^2
    dfx <- 2*x
    return(c(fx, dfx))
}

newtonraphson(ftn4, 10, 1e-06)

ftn4 <- function(x) {
    fx <- exp(x)
    dfx <- exp(x)
    return(c(fx, dfx))
}

newtonraphson(ftn4, 10, 1e-06,100)

curve(exp(x),0,15)

exp(-16)

