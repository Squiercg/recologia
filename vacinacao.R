##http://rpubs.com/kaz_yos/toc
##http://homepages.warwick.ac.uk/%7Emasfz/ModelingInfectiousDiseases/Chapter8/Program_8.2/index.html
##install.packages("deSolve")
library(deSolve)

times <- seq(from = 0, to = 600, by = 1)           
yinit <- c(Susc = 0.9, Infected = 0.1, Recovered = 0)
pars <- c(beta = 0.1, recovery = 0.005, death = 0.001, birth = 0.001)

SIR_model <- function(times, yinit, pars){    
    with(as.list(c(yinit,pars)), {
        dSusc      <- birth - beta*Infected*Susc                     - death*Susc
        dInfected  <-         beta*Infected*Susc - recovery*Infected - death*Infected
        dRecovered <-                              recovery*Infected - death*Recovered
        return(list(c(dSusc, dInfected, dRecovered)))})
    
    }

results <- ode(func = SIR_model, times = times, y = yinit, parms = pars)
## 
matplot(results[, 1], results[, 2:4], type="l", lty=1)
legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)


###

epi203v <- function(pars){

    ## NOTICE that this includes a new parameter "vaccination"

    ## Show parameters
    print(pars) 

    ## Additional parameters
    times <- seq(from = 0, to = 600, by = 1)
    yinit <- c(Susc=0.9, Infected=0.1, Recovered=0)  

    ## SIR model with vaccination (vaccine takes people from the Susceptible and put them in the Recovered)
    SIR_model <- function(times,yinit,pars){

    with(as.list(c(yinit,pars)), {

            dSusc      <- birth - beta*Infected*Susc                     - vaccination*Susc - death*Susc
            dInfected  <-         beta*Infected*Susc - recovery*Infected                    - death*Infected
            dRecovered <-                              recovery*Infected + vaccination*Susc - death*Recovered

            return(list(c(dSusc, dInfected, dRecovered)))}) 

    }

    ## run the ode solver for the function specified (function defined above is used)
    ## return the value of each compartment (Susc, Infected, Recovered) for each time step.
    results <- ode(func = SIR_model,times = times,y = yinit,parms = pars)
    results <- as.data.frame(results)

    ## Return results
    return(results)
}


test.pars.v <- c(beta = 0.1, recovery = 0.005, death = 0.001, birth = 0.001, vaccination = 0.1)
results.v   <- epi203v(test.pars.v)

       beta    recovery       death       birth vaccination 
      0.100       0.005       0.001       0.001       0.100 


## Plotting
matplot(x = results.v[,1], y = results.v[,2:4], type="l",lty=1)
legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)

plot of chunk unnamed-chunk-4 
