set.seed(123)
library(phytools)
# Simulando uma arvore de 5 espécies
arvore <- pbtree(n = 6)
arvore$tip.label <- paste("Sp",sub("t","",arvore$tip.label))
jpeg("01.jpg")
plot(arvore)
dev.off()

## 1 simulação de movimento browniano
simulacao <- fastBM(arvore,sig2=0.01,internal = TRUE)
jpeg("02.jpg")
phenogram(arvore,simulacao, spread.labels = TRUE, spread.cost = c(1, 0))
dev.off()

# 500 simulações de movimento browniano
saida <- fastBM(arvore, nsim = 500)

str(saida)
saida[,1:5]

#funções auxiliares para o pairs
#para plotar um histograma na diagonal do gráfico
panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
# Para colocar as correlações, com tamanho do texto proporcional ao valor.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r + 0.5)
}

jpeg("03.jpg")
pairs(t(saida),diag.panel = panel.hist,lower.panel = panel.smooth,upper.panel = panel.cor)
dev.off()

#vcv para correlação
vcv(arvore,model = "Brownian", corr = T)

#vcv para variancia-covariancia
vcv(arvore,model = "Brownian", corr = F)

#olhando o código do método vcv
vcv
methods(vcv)

vcv.phylo

#usando o prop.part
prop.part(arvore)

#Grafo
library(igraph)
dados.g <-graph.data.frame(arvore$edge, directed=F)
jpeg("04.jpg")
plot.igraph(dados.g,edge.label=round(arvore$edge.length,2),vertex.label=c(1:5,arvore$tip.label))
dev.off()
