#install.packages("ggplot2")
library("ggplot2")

dados<-data.frame(preditor=1:20,resposta=runif(20,5,10),grupo=rep(1:4,each=5),linha=runif(20,8,12))
head(dados)

## Gráfico de barras
ggplot(data=dados, aes(x=preditor, y=resposta)) + geom_bar(stat="identity")
ggsave("01.jpg")

## Separando por grupos
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(stat="identity")
##ggplot(data=dados, aes(x=preditor, y=resposta)) + geom_bar(aes(fill=grupo),stat="identity")
ggsave("02.jpg")

## Adicionando um contorno preto, e retirando a legenda
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE)
ggsave("03.jpg")

##Gráfico de linhas
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + guides(fill=FALSE)
ggsave("04.jpg")
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE)
ggsave("05.jpg")

##Combinando os dois
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE) +
    geom_line(data=dados, aes(x=preditor, y=linha)) + geom_point(data=dados, aes(x=preditor, y=linha))
ggsave("06.jpg")


##Transformando em uma figura circular
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE) +
    geom_line(data=dados, aes(x=preditor, y=linha)) + geom_point(data=dados, aes(x=preditor, y=linha))+coord_polar()
ggsave("07.jpg")

ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE) + coord_polar()
ggsave("08.jpg")
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE) + coord_polar() + ylim(0,12)
ggsave("09.jpg")
