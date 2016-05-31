###http://recologia.com.br/2016/05/relacao-especie-area/
library(ggplot2)

###Desenhando as curvas da relação espécie área
A<-10^(1:10)
c<-1.5
z<-0.25

fn<-function(x,c=1.5,z=0.25) {
    return(c*x^z)
}
ggplot(data.frame(x=c(10, 10^10)), aes(x)) + stat_function(fun=fn) + xlab(label="Área(ha)") + ylab(label="Número de espécies")
ggsave("01.jpg")


fn_log<-function(x,c=1.5,z=0.25) {
    return(log(c,10)+z*x)
}
ggplot(data.frame(x=c(1, 10)), aes(x)) + stat_function(fun=fn_log) + xlab(label="Área(ha)") + ylab(label="Número de espécies")
ggsave("02.jpg")

###Johnson, Mason, and Raven 1968
exemplo<-data.frame(Location = c("Tiburon Peninsula", "San Francisco","Santa Barbara area", "Santa Monica Mountains", "Marin County", 
                                 "Santa Cruz Mountains", "Monterey County", "San Diego County","California Coast"),
                    Area = c(5.9, 45, 110, 320, 529, 1386, 3324,4260, 24520),
                    Species = c(370L, 640L, 680L, 640L, 1060L, 1200L,1400L, 1450L, 2525L))
exemplo


ggplot(data=exemplo,aes(x=Area,y=Species))+ geom_point() + xlab(label="Área(ha)") + ylab(label="Número de espécies")
ggsave("03.jpg")

log(1000,10)

modelo<-lm(log(Species,10)~log(Area,10),data=exemplo)
summary(modelo)

modelo_nls<-nls(Species~a*Area^z,start=list(a=1,z=0.25),data=exemplo)
summary(modelo_nls)

linha<-data.frame(x=log10(seq(1,25000,100)),y=log10(predict(modelo_nls,newdata=data.frame(Area=seq(1,25000,100)))))



ggplot(data=exemplo,aes(x=log10(Area),y=log10(Species),linetype="solid") )  + geom_point() +
    geom_abline(intercept = modelo$coefficients[1], slope = modelo$coefficients[2]) +
    geom_line(data=linha,aes(x=x,y=y,linetype="dashed"))+        
    xlab(label="Área(ha)") + ylab(label="Número de espécies")+
    scale_linetype_manual(name='Modelo',values =c('dashed','solid'), labels = c('Não Linear','Linear'))
ggsave("04.jpg")

confint(modelo)
confint(modelo_nls)
