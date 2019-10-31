
##Dados
head(iris)

##Usando paris()
jpeg("01.jpg")
pairs(iris[,-5])
dev.off()

jpeg("011.jpg")
pairs(iris[,-5])
axis(1,at=0,labels=NA,tick=T)
arrows(0.18,0.85,0.18,0.61,lwd=6,col="red")
arrows(0.4,0.6,0.19,0.6,lwd=6,col="red")
dev.off()

jpeg("012.jpg")
pairs(iris[,-5])
axis(1,at=0,labels=NA,tick=T)
arrows(0.18,0.85,0.38,0.85,lwd=6,col="blue")
arrows(0.4,0.6,0.4,0.8,lwd=6,col="blue")
dev.off()

##Alterando a diagonal com uma função pré-definida
jpeg("02.jpg")
pairs(iris[,-5],lower.panel = panel.smooth)
dev.off()

##Definindo uma função para a diagonal superior
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(0, 1, 0, 1))
         r <- abs(cor(x, y))
         txt <- format(c(r, 0.123456789), digits = digits)[1]
         txt <- paste0(prefix, txt)
         if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
         text(0.5, 0.5, txt, cex = cex.cor * r)
}

##Usando a função definida
jpeg("03.jpg")
pairs(iris[,-5],lower.panel= panel.smooth,upper.panel = panel.cor)
dev.off()

##Alterando atributos via ...
jpeg("04.jpg")
pairs(iris[,-5],lower.panel= panel.smooth,upper.panel = panel.cor,pch=19)
dev.off()

jpeg("05.jpg")
pairs(iris[,-5],lower.panel= panel.smooth,upper.panel = panel.cor,pch=19,lwd=2)
dev.off()

##Definindo uma função para os gráficos da diagonal
panel.hist <- function(x, ...) {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$counts; y <- y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

##Utilizando a função para a diagonal.
jpeg("06.jpg")
pairs(iris[,-5],lower.panel= panel.smooth,upper.panel = panel.cor,diag.panel = panel.hist)
dev.off()
