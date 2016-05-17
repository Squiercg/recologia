install.packages("ggplot2")
library("ggplot2")

dados<-data.frame(preditor=1:20,resposta=runif(20,5,10),grupo=rep(1:4,each=5),linha=runif(20,8,12))

## Gráfico de barras
ggplot(data=dados, aes(x=preditor, y=resposta)) + geom_bar(stat="identity")


## Separando por grupos
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(stat="identity")
##ggplot(data=dados, aes(x=preditor, y=resposta)) + geom_bar(aes(fill=grupo),stat="identity")


## Adicionando um contorno preto, e retirando a legenda
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE)


##Gráfico de linhas
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + guides(fill=FALSE)
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE)


##Combinando os dois
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE) +
    geom_line(data=dados, aes(x=preditor, y=linha)) + geom_point(data=dados, aes(x=preditor, y=linha))



##Transformando em uma figura circular
ggplot(data=dados, aes(x=preditor, y=resposta,fill=grupo)) + geom_bar(colour="black",stat="identity") + guides(fill=FALSE) +
    geom_line(data=dados, aes(x=preditor, y=linha)) + geom_point(data=dados, aes(x=preditor, y=linha))+coord_polar()

ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE) + coord_polar()
ggplot(data=dados, aes(x=preditor, y=linha)) + geom_line() + geom_point() + guides(fill=FALSE) + coord_polar() + ylim(0,12)
