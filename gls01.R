library(nlme)

#Abrindo os dados do site do Crawley
spatialdata <- read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/spatialdata.txt",header=T)
str(spatialdata)

#Olhando os dados
table(spatialdata$variety)
#Figura 1
boxplot(yield~variety,data=spatialdata,frame=F)

#Anova das variedades
model1 <- aov(yield~variety,data=spatialdata)
summary(model1)

#Existe uma diferença entre blocos, então adicionamos eles na analise
tapply(spatialdata$yield,spatialdata$Block,mean)

Block <- factor(spatialdata$Block)
model2 <- aov(yield~Block+variety+Error(Block),data=spatialdata)
summary(model2)

#Figura 2 - Efeito do espaço
par(mfrow=c(2,1))
plot(spatialdata$latitude,spatialdata$yield,pch=19,xlab="Latitude",ylab="Produtividade")
smooter <- loess(yield~latitude,data=spatialdata,span=0.75)
lines(predict(smooter), col='red', lwd=2)

plot(spatialdata$longitude,spatialdata$yield,pch=19,xlab="Longitude",ylab="Produtividade")
smooter <- loess(yield~longitude,data=spatialdata,span=0.75)
lines(predict(smooter), col='red', lwd=2)

#Levando em conta a latitude e longitude
model3 <- aov(yield~Block+variety+latitude+longitude,data=spatialdata)
summary(model3)

#Figura 3 - Explorando melhor como é o experimento
paleta<-palette(rainbow(length(levels(spatialdata$variety))))
plot(spatialdata$latitude,spatialdata$longitude,col=paleta[spatialdata$variety],pch=19,
     cex=2*(spatialdata$yield/max(spatialdata$yield)),frame=F,xlab="Latitude",ylab="Longitude",xlim=c(4,50),ylim=c(0,30))
text(spatialdata$latitude+1.5,spatialdata$longitude,spatialdata$Block,cex=0.8)

#Modelo inicial gls
model4 <- gls(yield~variety,spatialdata)
anova(model4)

#Figura 4 - Variograma
plot(Variogram(model4,form=~latitude+longitude))
dev.off()

#Adicionando duas estruturas de correção espacial
model5 <- update(model4,corr=corSpher(c(28,0.2),form=~latitude+longitude,nugget=T))
anova(model5)

model6 <- update(model4,corr=corRatio(c(12.5,0.2),form=~latitude+longitude,nugget=T))
anova(model6)

#Comparando modelos
anova(model5,model6)
anova(model4,model6)

#Obsevando o resultado final
anova(model6)

#Figura 5 - Variograma do modelo final
plot(Variogram(model6,resType="n"))

#Figura 6 - Residuos
plot(model6,resid( ., type="n")~fitted(.),abline=0)
dev.off()

#Figura 7 - Distribuição dos residuos
hist(resid(model6))

#Analise de constrastes
levels(spatialdata$variety)
anova(model6,L=c(-1,0,1))
