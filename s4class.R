#post:
#http://recologia.com.br/2015/01/r-orientado-a-objetos-e-um-exemplo-s4

#definindo classe
setClass("armadilhaDeSementes",representation(distancias = "numeric",
                                       contagem.sementes = "numeric",
                                       area.armadilha = "numeric"))

#construtor
setMethod("initialize","armadilhaDeSementes",
          function(.Object,
                   distancias = numeric(0),
                   contagem.sementes = numeric(0),
                   area.armadilha = numeric(0)) {
    if (length(distancias) != length(contagem.sementes)) {
        stop("Quantidade de distâncias e contagens é diferente.")
    }
    if (length(area.armadilha) != 1) {
        stop("Área da armadilha é ambigua.")
    }
    .Object@distancias <- distancias
    .Object@contagem.sementes <- contagem.sementes
    .Object@area.armadilha <- area.armadilha
    .Object
})

#instanciando um objeto
s1 <- new("armadilhaDeSementes",distancias = 1:4,
          contagem.sementes = c(4, 3, 2, 0),
          area.armadilha = 0.0001)

#criando uma função para criar objetos s4
armadilhaDeSementes <- function(...) {
    new("armadilhaDeSementes",...)
}

#instanciando um objeto
s2<-armadilhaDeSementes(distancias=1:4,contagem.sementes=c(4, 3, 2, 0),
                 area.armadilha=0.0001)
class(s2)

#atributos da classe
slotNames(s1)

#acessando os atributos
s1@distancias


#definindo métodos
setMethod("show",signature(object = "armadilhaDeSementes"),
          function(object) {
              str(object)
          }
          )
#
setMethod("mean",signature(x = "armadilhaDeSementes"),
          function(x, ...) {
              weighted.mean(x@distancias,w = x@contagem.sementes)
          }
          )

#usando método show
s1

#método generico
mean(s1)

#métodos s3 de mean
methods(mean)

#métodos com assinatura s4
showMethods("mean")

#métodos da classe
showMethods(classes = "armadilhaDeSementes")

#vendo o código do método mean da classe armadilhaDeSementes
getMethod("mean", "armadilhaDeSementes")
