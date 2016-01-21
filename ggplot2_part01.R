#http://recologia.com.br/2015/11/o-que-e-ggplot2/
library(ggplot2)

preditor<-runif(100,1,5)
resposta<-rnorm(100,2+2*preditor)
dados<-data.frame(preditor,resposta)


p <- ggplot(dados,aes(preditor,resposta))
p <- p + layer(geom = "point")
p
ggsave("figura01.jpg")


p <- ggplot(dados,aes(x=preditor))
p <- p + layer(geom = "bar",geom_params = list(fill = "steelblue"),stat = "bin",stat_params = list(binwidth = 0.2))
p
ggsave("figura02.jpg")

p <- ggplot(dados,aes(x=preditor))
p <- p + geom_histogram(binwidth = 0.2, fill = "steelblue")
p
ggsave("figura03.jpg")


p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
ggsave("figura04.jpg")
