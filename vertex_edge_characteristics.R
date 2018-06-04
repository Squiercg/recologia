library(igraph)
library(igraphdata)
library(sand)
library(network)

##Dados karate
data(karate)
jpeg("01.jpg",width=480,height = 480)
plot(karate)
dev.off()


##Distribuição de graus
jpeg("02.jpg",width=1.5*480,height = 480)
par(mfrow=c(1,2))
plot(karate)
hist(degree(karate), col="lightgray", xlim=c(0, 50),xlab="Grau dos vértices", ylab="Frequência", main="")
dev.off()

##Distribuição de graus com peso
jpeg("03.jpg",width=1.5*480,height = 480)
par(mfrow=c(1,2))
plot(karate)
hist(graph.strength(karate), col="gray",xlab="Peso dos vértices", ylab="Frequência", main="")
dev.off()

##Dados fermento
data(yeast)
jpeg("04.jpg",width=480,height = 480)
plot(yeast,vertex.label=NA,vertex.size=5,vertex.color="grey")
dev.off()
ecount(yeast)
vcount(yeast)

##Distribuição graus fermento
d.yeast <- degree(yeast)
jpeg("05.jpg",width=480,height = 480)
hist(d.yeast,col="gray",xlab="GRau", ylab="Frequência",main="Distribuição de graus")
dev.off()

##Distribuição graus fermento (log)
jpeg("06.jpg",width=1.5*480,height = 480)
par(mfrow=c(2,1))
hist(d.yeast,col="gray",xlab="DegreeGRau", ylab="Frequência",main="Distribuição de graus")
dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="gray",xlab=c("Log dos graus"), ylab=c("Graus IntensidadeLog"),main="Distribuição de grau Log-Log")
dev.off()



##Average neighbor degree versus vertex degree (log–log scale)
a.nn.deg.yeast <- graph.knn(yeast,V(yeast))$knn
jpeg("07.jpg",width=480,height = 480)
plot(d.yeast, a.nn.deg.yeast, log="xy",col="gray", xlab="Log do grau do vértice",ylab="Log da média do gráu dos vizinhos")
dev.off()


##Degree, Closeness, Betweenness e Eigenvalue
A <- get.adjacency(karate, sparse=FALSE)
g <- network::as.network.matrix(A)
jpeg("08.jpg",width=480,height = 480)
sna::gplot.target(g, degree(g), main="Degree",circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),edge.col="darkgray")
dev.off()
jpeg("09.jpg",width=480,height = 480)
sna::gplot.target(g, closeness(g), main="Closeness",circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),edge.col="darkgray")
dev.off()
jpeg("10.jpg",width=480,height = 480)
sna::gplot.target(g, betweenness(g), main="Betweenness",circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),edge.col="darkgray")
dev.off()
jpeg("11.jpg",width=480,height = 480)
sna::gplot.target(g, evcent(g)$vector, main="Eigenvalue",circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),edge.col="darkgray")
dev.off()

##HITS algorithm
library(sna)
l <- layout.kamada.kawai(aidsblog)
jpeg("12.jpg",width=480,height = 480)
plot(aidsblog, layout=l, main="Hubs", vertex.label="",vertex.size=10 * sqrt(hub.score(aidsblog)$vector))
dev.off()
jpeg("12.jpg",width=480,height = 480)
plot(aidsblog, layout=l, main="Autoridades",vertex.label="", vertex.size=10 *sqrt(authority.score(aidsblog)$vector))
dev.off()
