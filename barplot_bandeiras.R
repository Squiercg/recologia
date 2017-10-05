library(png)
##Git das bandeiras
##https://github.com/gosquared/flags
paises<-list.files("64")



##Figura country.png

country<-c("Brazil","United States","Portugal","Mozambique","Mexico","Germany","Peru","Australia","Cape Verde","Angola")
country_br<-c("Brasil","Estados Unidos","Portugal","Mozambique","México","Alemanha","Perú","Austrália","Cabo Verde","Angola")
sessions<-c(2113,259,85,63,54,49,38,37,36,36)
sessions_percentage<-c(60.39,7.4,2.43,1.8,1.54,1.4,1.09,1.06,1.03,1.03)

ampliacao <- 2
jpeg("country.jpg",width = ampliacao*480,height = ampliacao*480,pointsize = ampliacao*12)
escala<-barplot(rev(sessions_percentage),horiz=T,xlim=c(-150,100),xaxt="n",col="#76A7FA")
text(rep(0,10),escala,rev(sessions),pos=2)
text(rev(sessions_percentage)+14,escala,paste(rev(format(round(sessions_percentage, 2), nsmall = 2)),"%",sep=""))
text(rep(-130,10),escala,10:1,pos=2)
text(rep(-120,10),escala,rev(country_br),pos=4)
mtext(c("País","Sessões","% Sessões"),3,0,at=c(-125,-15,35))

list.files()

for(i in 1:10){
    country[i]
    posicao<-agrep(country[i],paises)
    paises[posicao]
    bandeira<-readPNG(paste("64/",paises[posicao],sep=""))
    rasterImage(bandeira,-133,rev(escala)[i]-0.5,-117,rev(escala)[i]+0.5)
}
dev.off()
