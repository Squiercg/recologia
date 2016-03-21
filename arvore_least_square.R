###http://recologia.com.br/2016/03/least-squares-para-arvores-filogeneticas/
rm(list=ls())
##install.packages("phangorn")
library(phangorn)


###Gerando um arquivo com sequencias
cat(">A",
    "AAAAA",
    ">B",
    "CAAAA",
    ">C",
    "CGGGG",
    ">D",
    "GGGGG",
file = "exemplo.fasta", sep = "\n")

###Lendo sequencias e transformando na classe phyDat
ex.dna <- read.dna("exemplo.fasta", format = "fasta")
dist.dna(ex.dna,model="raw")

ex.phyDat<-as.phyDat(ex.dna)


#####################################
## Topologia 1
#####################################
###Criando uma árvore
arvore1 <- "((A:1,B:1):1,(C:1,D:1):1);"
arvore1.tre <- read.tree(text=arvore1)

###Como é árvore
plot(arvore1.tre)
###Calculando o ajuste
ajuste1<-pml(arvore1.tre, data=ex.phyDat,model="JC")
ajuste1

#####################################
## Topologia 2
#####################################
###Agora uma topologia ruim
arvore2 <- "((C:1,B:1):1,(A:1,D:1):1);"
arvore2.tre <- read.tree(text=arvore2)

###Como é árvore
plot(arvore2.tre)
ajuste2<-pml(arvore2.tre, data=ex.phyDat,model="JC")

ex.dna <- read.dna("exemplo.fasta", format = "fasta",as.character=T,as.matrix=F)

arvore1.tre$tip.label<-sapply(ex.dna,paste,collapse = "")[match(arvore1.tre$tip.label,names(ex.dna))]
arvore2.tre$tip.label<-sapply(ex.dna,paste,collapse = "")[match(arvore2.tre$tip.label,names(ex.dna))]

par(mfrow=c(1,2),mar=c(0,0,2,0))
plot(arvore1.tre,main=paste("Verossimilhança:",round(ajuste1$logLik,2)),type="unrooted")
plot(arvore2.tre,main=paste("Verossimilhança:",round(ajuste2$logLik,2)),type="unrooted")

format(exp(ajuste2$logLik),scientific = F)
