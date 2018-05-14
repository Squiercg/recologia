##source("http://bioconductor.org/biocLite.R")
##biocLite()
##biocLite(c("graph", "RBGL", "Rgraphviz"))
##install.packages("gRain")

library(graph)
library(gRain)
library(gRbase)
library(Rgraphviz)

graph <- ug(~A:B:E + C:E:D)
class(graph)


plot(graph)

dag <- dag(~A + B:A + C:B + D:B + E:C:D)
dag
plot(dag)



machine_val <- c("working","broken")
light_bulb_val <- c("good","bad")

machine_prob <- c(99,1)
light_bulb_prob <- c(99,1,60,40)

M <- cptable(~machine, values=machine_prob, levels=machine_val)
L <- cptable(~light_bulb|machine, values=light_bulb_prob, levels=light_bulb_val)

plist <- compileCPT(list(M,L))
plist

plist$machine
plist$light_bulb

net <- grain(plist)
net2 <- setEvidence(net, evidence=list(light_bulb="bad"))
querygrain(net2, nodes=c("machine"))
