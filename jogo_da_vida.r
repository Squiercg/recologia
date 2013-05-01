#########################################################################
#Jogo da vida
#Script Original:
#http://johnramey.net/blog/2011/06/05/conways-game-of-life-in-r-with-ggplot2-and-animation/
#########################################################################
#install.packages("foreach")
library(foreach)

#Determina quantos vizinhos cada celula da matriz tem.
#Serve para implementar as regras do jogo

how_many_neighbors <- function(grid, j, k) {
  size <- nrow(grid)
  count <- 0
  if(j > 1) {
    count <- count + grid[j-1, k]
    if (k > 1) count <- count + grid[j-1, k-1]
    if (k < size) count <- count + grid[j-1, k+1]
  }
  if(j < size) {
    count <- count + grid[j+1,k]
    if (k > 1) count <- count + grid[j+1, k-1]
    if (k < size) count <- count + grid[j+1, k+1]
  }
  if(k > 1) count <- count + grid[j, k-1]
  if(k < size) count <- count + grid[j, k+1]
  count
}

#Retorna uma lista de matrizes, após a primeira, cada matriz é uma iteração.
#size: Tamanho da matriz
#num_reps: número de repetições
#prob: Chance de vida e morte para a configuração inicial.

game_of_life <- function(size=50,num_reps=10,prob=c(0.5,0.5),inicial=NA) {
  grid <- list()
  if(is.na(inicial)) {
    grid[[1]] <- replicate(size, sample(c(0,1), size, replace = TRUE, prob = prob))
  } else {
    grid[[1]] <- inicial
  }
  dev_null <- foreach(i = seq_len(num_reps) + 1) %do% {
    grid[[i]] <- grid[[i-1]]
    foreach(j = seq_len(size)) %:%
      foreach(k = seq_len(size)) %do% {

        #Aplicar as Regras de Conway
        num_neighbors <- how_many_neighbors(grid[[i]], j, k)
        alive <- grid[[i]][j,k] == 1
        if(alive && num_neighbors <= 1) grid[[i]][j,k] <- 0
        if(alive && num_neighbors >= 4) grid[[i]][j,k] <- 0
        if(!alive && num_neighbors == 3) grid[[i]][j,k] <- 1
      }
  }
  grid
}

game_grids <- game_of_life(size = 50, num_reps = 20, prob = c(0.9, 0.1))


#Plotando cada matriz da lista com image, usando o Sys.sleep para ter um intervalo
#por plot

for(i in 1:length(game_grids)) {
  image(game_grids[[i]],col=c("white","black"),xaxt="n",yaxt="n")
  box()
  title(paste("Geração ",i))
  Sys.sleep(time=1)
}
