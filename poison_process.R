jpeg("01.jpg")
plot(1:30,dpois(1:30,10),type="l",lty=3,lwd=3,col="red",frame=F,xlab="x",ylab="Probabilidade")
points(1:30,dnorm(1:30,10,sqrt(10)),type="l",lty=3,lwd=3,col="blue")
legend("topright",legend=c("Poisson","Normal"),lty=3,lwd=3,col=c("red","blue"),bty="n")
dev.off()

rm(list=ls())
tamanho_grid <- 10
unidade_amostra <- 2
intensidade <- 1

exp.M <- intensidade * tamanho_grid^2
quebras <- seq(0, tamanho_grid, unidade_amostra)
n_amostras <- (tamanho_grid/unidade_amostra)^2
ponto_meio <- quebras[-length(quebras)] + 0.5 * unidade_amostra
M <- rpois(1, exp.M)
eixo_x <- runif(M, 0, tamanho_grid)
eixo_y <- runif(M, 0, tamanho_grid)
N <- as.matrix(table(cut(eixo_x, breaks = quebras), cut(eixo_y, breaks = quebras)))
lambda <- round(mean(N), 2)
var <- var(c(N))
z <- N
z[z > 1] <- 1
psi <- mean(z)

###Figura 1
plot(eixo_x, eixo_y, xlab = "x coord", ylab = "y coord", cex = 1,pch = 16, asp = 1,
     main = paste("Point pattern: \nIntensidade =",intensidade, ", M =", M, "inds."),
     xlim = c(0,tamanho_grid), ylim = c(0, tamanho_grid), frame = F,col = "red")
polygon(c(0, tamanho_grid, tamanho_grid, 0), c(0, 0, tamanho_grid,tamanho_grid), lwd = 3, col = NA, border = "black")

###Figura 2
plot(eixo_x, eixo_y, xlab = "x coord", ylab = "y coord", cex = 1,pch = 16, asp = 1,
     main = paste("Abundance pattern: \nRealized mean density =",lambda, "\nSpatial variance =", round(var, 2)),
     xlim = c(0, tamanho_grid), ylim = c(0, tamanho_grid), frame = F,col = "red")
##
for (i in 1:length(quebras)) {
    for (j in 1:length(quebras)) {
        segments(quebras[i], quebras[j], rev(quebras)[i],quebras[j])
        segments(quebras[i], quebras[j], quebras[i], rev(quebras)[j])
    }
}
##
for (i in 1:length(ponto_meio)) {
    for (j in 1:length(ponto_meio)) {text(ponto_meio[i], ponto_meio[j], N[i, j], cex = 10^(0.8 -0.4 * log10(n_amostras)), col = "blue")
    }
}
polygon(c(0, tamanho_grid, tamanho_grid, 0), c(0, 0, tamanho_grid,tamanho_grid), lwd = 3, col = NA, border = "black")

##Figura 3
plot(eixo_x, eixo_y, xlab = "x coord", ylab = "y coord", cex = 1,pch = 16, asp = 1,
     main = paste("Occurrence pattern: \nRealized occupancy =",round(psi, 2)), xlim = c(0, tamanho_grid),
     ylim = c(0,tamanho_grid), frame = F, col = "red")
##
for (i in 1:length(quebras)) {
    for (j in 1:length(quebras)) {
        segments(quebras[i], quebras[j], rev(quebras)[i],quebras[j])
        segments(quebras[i], quebras[j], quebras[i], rev(quebras)[j])
    }
}
##
for (i in 1:(length(quebras) - 1)) {
    for (j in 1:(length(quebras) - 1)) {
        polygon(c(quebras[i], quebras[i + 1], quebras[i +1], quebras[i]),
                c(quebras[j], quebras[j], quebras[j +1],quebras[j + 1]),
                col = "black", density = z[i,j] * 100)
    }
}
polygon(c(0, tamanho_grid, tamanho_grid, 0), c(0, 0, tamanho_grid,tamanho_grid), lwd = 3, col = NA, border = "black")

##Figura 4
plot(table(N), xlab = "Abundance (N)", ylab = "Number of cells",col = "black",
     xlim = c(0, max(N)), main = "Frequency of N with mean density (blue)",lwd = 3, frame = F)
abline(v = lambda, lwd = 3, col = "blue")
