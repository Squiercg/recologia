##http://recologia.com.br/2018/05/simulando-dados-espaciais-com-o-pacote-geor/


##install.packages("geoR")
library(geoR)

##Gerando dados
set.seed(7)
dados <-grf(50, cov.pars=c(1, .25))

## Resultado da simulação
dados

## O que temos no objeto
names(dados)

## Metodo plot padrão do geoR
plot(dados)


## Extraindo informações
dados$coords
dados$data

## Visualizando com um data frame, talvez o mais comum numa planilha.
data.frame(Eixo_x=dados$coords[,1],Eixo_y=dados$coords[,2],Medida=dados$data)

##Plotando os dados
points.geodata(dados)

## Ajustando um modelo
modelo <- likfit(dados,ini=c(0.5, 0.5))
modelo
summary(modelo)

## Exemplo de Krikagem
pred.grid <- expand.grid(seq(0, 1, l = 51), seq(0, 1, l = 51)) 
kc <- krige.conv(dados, loc = pred.grid, krige = krige.control(obj.m = modelo))
image(kc, loc = pred.grid, col = gray(seq(1, 0.1,l = 30)), xlab = "Coordenadas X", ylab = "Coordenadas Y")
