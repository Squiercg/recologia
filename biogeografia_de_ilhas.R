###http://recologia.com.br/2016/06/biogeografia-de-ilhas/
library(ggplot2)

exemplo<-data.frame(especies=seq(0.1,50,0.2))

##Imigração
IO<-log(1)
b<-0.1
exemplo$imigracao<-exp(IO-b*exemplo$especies)

ggplot(data=exemplo,aes(x=especies,y=imigracao))+geom_line()+
    xlab("Número de espécies") +
    ylab("Taxa de imigração")
ggsave("01.jpg")
##curve(exp(IO-b*x),0,50,xlab="n. de espécies",ylab="Taxa Imigração")

##Extinção
d<-0.01
exemplo$extincao<-exp(d*exemplo$especies)-1
ggplot(data=exemplo,aes(x=especies,y=extincao))+geom_line()+
    xlab("Número de espécies") +
    ylab("Taxa de extinção")
ggsave("02.jpg")
##curve(exp(d*x)-1,0,50,xlab="n. de espécies",ylab="Taxa Extinção")


##Os dois juntos
exemplo2<-stack(exemplo[,c("imigracao","extincao")])
colnames(exemplo2)<-c("valores","tipo")
exemplo2$especies<-rep(exemplo$especies,2)

ggplot(data=exemplo2,aes(x=especies,y=valores,group=tipo,colour=tipo))+geom_line()+
    scale_colour_discrete(name="Taxas",labels=c("Extinção","Imigração"))+
    xlab("Número de espécies") +
    ylab("Taxa")
ggsave("03.jpg")
##
deltaR<-function(R){
    (exp(IO-b*R) - (exp(d*R)-1))^2
}
estavel<-optimize(f=deltaR,interval = c(1,50))
segmento<-data.frame(x = c(estavel$minimum,estavel$minimum),y=c(0,max(apply(exemplo[,2:3],1,min))))

ggplot(data=exemplo2,aes(x=especies,y=valores,group=tipo,colour=tipo))+geom_line()+
    scale_colour_discrete(name="Taxas",labels=c("Extinção","Imigração"))+
    geom_line(aes(x = x, y = y,group="Estável", colour = "Estável"),data = segmento,colour="black")+
    geom_point(aes(x = x, y = y,group="Estável", colour = "Estável"), data = segmento[2,],colour="black")+
    xlab("Número de espécies") +
    ylab("Taxa")
ggsave("04.jpg")



exemplo<-data.frame(especies=seq(0.1,50,0.2))
exemplo$isp1<-exp(IO-0.1*exemplo$especies)
exemplo$isp2<-exp(IO-0.14*exemplo$especies)
exemplo$extincao<-exp(d*exemplo$especies)-1
exemplo2<-stack(exemplo[,c("isp1","isp2","extincao")])
colnames(exemplo2)<-c("valores","tipo")
exemplo2$especies<-rep(exemplo$especies,3)


deltaR<-function(R,b){
    (exp(IO-b*R) - (exp(d*R)-1))^2
}
b<-0.1
estavel<-optimize(f=deltaR,interval = c(1,50),b=b)
segmento1<-data.frame(x = c(estavel$minimum,estavel$minimum),y=c(0,max(apply(exemplo[,c(2,4)],1,min))))
b<-0.14
estavel<-optimize(f=deltaR,interval = c(1,50),b=b)
segmento2<-data.frame(x = c(estavel$minimum,estavel$minimum),y=c(0,max(apply(exemplo[,c(3,4)],1,min))))



ggplot(data=exemplo2,aes(x=especies,y=valores,group=tipo,colour=tipo))+geom_line()+
    scale_colour_discrete(name="Taxas",labels=c("Extinção","Imigração Sp1","Imigração Sp2"))+
    geom_line(aes(x = x, y = y,group="Estável", colour = "Estável"),data = segmento1,colour="black")+
    geom_point(aes(x = x, y = y,group="Estável", colour = "Estável"), data = segmento1[2,],colour="black")+
    geom_line(aes(x = x, y = y,group="Estável", colour = "Estável"),data = segmento2,colour="black")+
    geom_point(aes(x = x, y = y,group="Estável", colour = "Estável"), data = segmento2[2,],colour="black")+
    xlab("Número de espécies") +
    ylab("Taxa")
ggsave("05.jpg")
