data.fn <- function(N = 100, mean.p = 0.5, T = 3, time.eff = runif(T,-2,2)){
    yfull <- yobs <- array(NA, dim = c(N, T) )
    p.vec <- array(NA, dim = T)
    for (j in 1:T){
        p <- plogis(log(mean.p / (1-mean.p)) + time.eff[j])
        yfull[,j] <- rbinom(n = N, size = 1, prob = p)
        p.vec[j] <- p
    }
    ever.detected <- apply(yfull, 1, max)
    C <- sum(ever.detected)
    yobs <- yfull[ever.detected == 1,]
    cat(C, "out of", N, "animals present were detected.\n")
    return(list(N = N, p.vec = p.vec, C = C, T = T, yfull = yfull, yobs =yobs))
}


data <- data.fn()

# Augment data set
nz <- 150
yaug <- rbind(data$yobs, array(0, dim = c(nz, data$T)))
# Specify model in BUGS language
sink("model.txt")
cat("
    model {
        # Priors
        omega ~ dunif(0, 1)
        for (i in 1:T){
            p[i] ~ dunif(0, 1)
        }
        # Likelihood
        for (i in 1:M){
            z[i] ~ dbern(omega)
            for (j in 1:T){
                yaug[i,j] ~ dbern(p.eff[i,j])
                p.eff[i,j] <- z[i] * p[j]
                } #j
            } #i
        # Derived quantities
        N <- sum(z[])
    }
    ",fill = TRUE)
sink()

# Bundle data
win.data <- list(yaug = yaug, M = nrow(yaug), T = ncol(yaug))

# Initial values
inits <- function() list(z = rep(1, nrow(yaug)), p = runif(data$T, 0, 1))
# Parameters monitored
params <- c("N", "p", "omega")
# MCMC settings
ni <- 2500
nt <- 2
nb <- 500
nc <- 3
# Call WinBUGS from R (BRT <1 min)
library(R2OpenBUGS)
out <- bugs(win.data, inits, params, "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
# Summarize posteriors
print(out, dig = 3)
hist(out$sims.list$N, nclass = 40, col = "gray", main = "", xlab = "Population size", las = 1, xlim = c(70, 150))
abline(v = data$C, col = "black", lwd = 3)

vetor1<-paste("corte_estrada_aneis",1:20,".dbf",sep="")
vetor1


vetor2<-sort(vetor1)
vetor2

numeros<-as.numeric(gsub("[a-z_.]", "\\1", vetor2))
numeros
vetor2

order(numeros)

numeros[order(numeros)]

vetor2[order(numeros)]
