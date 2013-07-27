#Instalando e abrindo o pacote para Phylogenia no R
#install.packages("ape")
library(ape)

#Criando uma lista com números de acesso ao Genbank
lista <- paste("AF006", seq(387, 459, 2), sep = "")
lista

#Fazendo Download pela internet
felidseq16S <-read.GenBank(lista)

#Olhando os dados
felidseq16S

table(unlist(lapply(felidseq16S, length)))
cat(felidseq16S[[1]], "\n", sep = "")

#Com letrinhas
read.GenBank(lista[1],as.character = T)

#Salvando o arquivo para alinhas
write.dna(felidseq16S, "felidseq16S.fas", format = "fasta")

#Apos o alinhamento, abra novamente e de uma olhada
felidseq16Sali <- read.dna("felidae16s.phylip")
table(unlist(lapply(felidseq16Sali, length)))

#Salvando os nomes das especies também
taxa.felid <- attr(felidseq16S, "species")
names(taxa.felid) <- names(felidseq16S)

#Desenhando as arvores, lembre-se de indicar corretamente o diretorio e o executavel do PhyML
getwd()
phymltest.felid <- phymltest(seqfile="felidae16s.phylip",
                             execname="/home/augusto/git/recologia/PhyML-3.1/PhyML-3.1_linux32",append = F)

#O resultado
phymltest.felid

#Testando diferenças nos modelos usados
summary(phymltest.felid)

#Visualizando os ajustes
plot(phymltest.felid)

#Abrindo a arvore que queremos olhar, definindo a raiz e tirando ela do plot
tr <- read.tree("felidae16s.phylip_phyml_tree.txt")
mltree.felid <- tr[[28]]
mltree.felid$tip.label <- taxa.felid[mltree.felid$tip.label]
mltree.felid <- root(mltree.felid, "Galidia_elegans")
mltree.felid <- drop.tip(mltree.felid, c("Crocuta_crocuta",
"Galidia_elegans"))

#Plots das arvores
plot(mltree.felid)
axisPhylo()

plot(mltree.felid,type="cladogram")

plot(mltree.felid,type="fan")

plot(mltree.felid,type="radial")

plot(mltree.felid,type="unrooted",cex=0.6)




