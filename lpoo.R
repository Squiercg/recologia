mean
methods(mean)

objeto1<-c(30,1,2,3)
class(objeto1)
mean(objeto1)


datas<- c("30/01/14", "01/02/14", "02/02/14", "03/02/14")
objeto2<-as.Date(datas, "%d/%m/%y")
class(objeto2)
mean(objeto2)

methods(plot)


#Parametros para desenhar um circulo.
#a e b são o ponto central, r é o raio e t é a variavel parametrica que vai de 0 a 2pi
t<-seq(0, 2*pi, length = 100)
r<-1
a<-0
b<-0

plot(a+r*cos(t),b+r*sin(t),type="l",xlim=c(-2,5),ylim=c(-3,3))
points(0,0,pch=19,col="green")
points(c(0.5,1,1.5,2,2.5),rep(0,5),pch=22,cex=2)

methods(class = "ArmadilhaDeSementes")

ArmadilhaDeSementes <- function(distancias, contagem.sementes, area.armadilha = 0.0001) {
    if (length(distancias) != length(contagem.sementes))
        stop("Quantidade de distancias e contagens é diferente.")
    if (length(area.armadilha) != 1) stop("Area da armadilha é ambigua.")
    ArmadilhaDeSementes <- list(distancias = distancias,
                         contagem.sementes = contagem.sementes,
                         area.armadilha = area.armadilha)
    class(ArmadilhaDeSementes) <- "ArmadilhaDeSementes"
    return(ArmadilhaDeSementes)
}

print.ArmadilhaDeSementes <- function(objeto, ...) {
    cat(paste("Distâncias: ",paste(objeto$distancias,collapse=", "),"\n"))
    cat(paste("Contagens : ",paste(objeto$contagem.sementes,collapse=", "),"\n"))
    cat(paste("Area da armadilha: ",objeto$area.armadilha,"\n"))
}

mean.ArmadilhaDeSementes <- function(x, ...) {
    return(weighted.mean(x$distancias, w = x$contagem.sementes))
}

methods(class = "ArmadilhaDeSementes")
methods(mean)
methods(print)

s1 <- ArmadilhaDeSementes(distancias = 1:4, contagem.sementes = c(4, 3, 2, 0))
s1
mean(s1)

class(s1)

is.list(s1)

s1[[1]]

help("::")
envir

search()

iris

detach("package:datasets", unload=TRUE)
search()

iris

datasets::iris

