##install.packages("igraph")
library(igraph)

# your data
mat <- as.matrix(read.table(text=
"node X1 X2 X3 X4 X5 X6
   1  0  3  7  4 NA NA
    2  3  0  2 NA NA  9
    3  7  2  0  1  3  6
    4  4 NA  1  0  3 NA
    5 NA NA  3  3  0  3
    6 NA  9  6 NA  3  0", header=T))

# prepare data for graph functions - set NA to zero to indicate no direct edge
nms <- mat[,1]
mat <- mat[, -1]
colnames(mat) <- rownames(mat) <- nms
mat[is.na(mat)] <- 0


# create graph from adjacency matrix
g <- graph.adjacency(mat, weighted=TRUE)
plot(g)



# Get all path distances
shortest.paths(g, algorithm = "dijkstra",v=1,t=5)



g <- make_ring(10)
plot(g)
distances(g)
shortest_paths(g,5,1)
all_shortest_paths(g, 1, 6:8)
mean_distance(g)
## Weighted shortest paths
el <- matrix(nc=3, byrow=TRUE,
             c(1,2,0, 1,3,2, 1,4,1, 2,3,0, 2,5,5, 2,6,2, 3,2,1, 3,4,1,
               3,7,1, 4,3,0, 4,7,2, 5,6,2, 5,8,8, 6,3,2, 6,7,1, 6,9,1,
               6,10,3, 8,6,1, 8,9,1, 9,10,4) )
g2 <- add_edges(make_empty_graph(10), t(el[,1:2]), weight=el[,3])
plot(g2)
distances(g2, mode="out")

shortest_paths(g2,from=4,to=9,algorithm="dijkstra")


