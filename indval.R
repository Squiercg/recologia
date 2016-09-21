###http://recologia.com.br/2016/09/especies-indicadores-de-agrupamento-indicator-value-ou-indval/
library(labdsv)
library(indicspecies)

set.seed(123)
agrupamento <- factor(rep(c("Conservado","Pertubado"),each=15))

n_amostras<-30
n_sp<-6

sp1<-rpois(n_amostras,1)
sp2<-rpois(n_amostras,0.5)
sp3<-rpois(n_amostras,(as.numeric(agrupamento)-1)*2)
sp4<-rpois(n_amostras,(as.numeric(agrupamento)-0.65))
sp5<-rpois(n_amostras,(as.numeric(agrupamento)-2)*-2)
sp6<-rpois(n_amostras,rev(as.numeric(agrupamento)-0.65))

comunidade<-as.data.frame(matrix(c(sp1,sp2,sp3,sp4,sp5,sp6),ncol=n_sp,nrow=n_amostras,byrow=F,
                                 dimnames=list(paste("Local",1:n_amostras),paste("Sp",1:n_sp))))
comunidade
dis.comunidade <- dsvdis(comunidade,'bray/curtis')
cluster<-hclust(dis.comunidade,method="complete")

##
jpeg("01.jpg")
plot(cluster)
dev.off()
##
jpeg("02.jpg")
plot(cluster,labels=agrupamento)
dev.off()

pca_comunidade<-princomp(comunidade)

jpeg("03.jpg")
plot(scores(pca_comunidade)[,1],scores(pca_comunidade)[,2],type="n",xlab="Eixo 1",ylab="Eixo2",frame=F)
text(scores(pca_comunidade)[,1],scores(pca_comunidade)[,2],agrupamento)
abline(v=0,col="red",lty=3,lwd=2)
dev.off()

jpeg("04.jpg")
biplot(pca_comunidade)
dev.off()

agrupamento
indval(comunidade,agrupamento)


indval <- multipatt(comunidade, agrupamento,control = how(nperm=5000))
summary(indval)
