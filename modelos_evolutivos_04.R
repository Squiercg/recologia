rm(list=ls())
# Remove all objects from memory
# Function to do numerical integration
INTEGRAND <- function(age,x) {
    Af <- 0
    Bf <- 8
    As <- 1
    Bs <- 0.5
    # parameter values
    return ((Af-Bf*x)*exp(-(As+Bs*x)*age))
    # return function
}

# MAIN PROGRAM
n<- 100
z<- seq(0,3,length=n)
W<- matrix(0,n,1)
for (i in 1:n) {
    # Iterate over n “body sizes”
    x<- z[i]
    # Set value of x
    # Integrate from 1 to infinity and add to W
    W[i] <- integrate(INTEGRAND,1,Inf,x)$value
}

plot(z,-W,type="l", xlab="Body size, x", ylab="Fitness, W",las=1,lwd=4)

##################################
## Texto...
##################################
y <- deriv(~(0+4*x)*exp(-(1+0.5*x))/(1+0.5*x),"x")
y


FUNC <- function(x){
    return(4+0.5*(0-4*x)-(0+4*x)*0.5/(1+0.5*x))
}
B <- uniroot(FUNC, interval= c(0,4))# Set lower interval 1⁄4 0
B$root


# Function to obtain the gradient at a value w
FUNC <- function(w) {
    y <- deriv(~(0+4*x)*exp(-(1+0.5*x))/(1+0.5*x),"x") # Get the derivative
    x <- w
    # Set x equal to w
    z <- eval(y)
    # Evaluate the derivative at w
    d <- attr(z,"gradient")
    # Assign the gradient value to d
    return(d)
    # Return d to the main program
}

# MAIN PROGRAM
# Root must be enclosed by the limits set by the user, here set at 0 to 4
B <- uniroot(FUNC, interval= c(0,4))
B$root
# Print out the value found

rm(list=ls())
# Remove all objects from memory
# Function to supply components for numerical integration
INTEGRAND <- function(age,x)  {
    Af <- 0
    Bf <- 4
    As <- 1
    Bs <- 0.5 # parameter values
    return (-(Af+Bf*x)*exp(-(As+Bs*x)*age))# return function value
}
# Function to call integration routine
FUNC <- function(x){integrate(INTEGRAND,1,Inf,x)$value}
# Minimization routine
nlm(FUNC,p=1)$estimate
