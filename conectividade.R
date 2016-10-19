##http://recologia.com.br/2016/10/mapas-aleatorios-e-o-numero-de-componentes/
#####################################
## Conectividade
#####################################
library(igraph)

##Função para gerar mapas quadrados aleatorios, n é o tamanho e p é a probabilidade de threshold
gera_mapa<-function(n,p) {
    matriz<-matrix(0,n,n)
    for(i in 1:nrow(matriz)){
        for(j in 1:ncol(matriz)){
            if(runif(1)<=p){
                matriz[i,j]<-1
            }
        }
    }
    return(matriz)
}

##Função para transformar um mapa em um grafo, com pixels com vértices e pontos adjacentes ligados por arestas
matriz_para_grafo<-function(matriz){
    aresta<-1
    grafo<-data.frame(origem=vector(),destino=vector())
    for(i in 1:nrow(matriz)){
        for(j in 1:ncol(matriz)){        
            if(matriz[i,j]!=0){            
                for(m in -1:1){
                    for(n in -1:1){
                        if(i+m>0 & i+m<=nrow(matriz) &
                           j+n>0 & j+n<=ncol(matriz)){
                            if(matriz[i+m,j+n]!=0){
                                grafo[aresta,]<-c(paste("(",i,",",j,")",sep=""),paste("(",i+m,",",j+n,")",sep=""))
                                aresta<-aresta+1
                            }
                        }
                    }
                }
            }
        }
    }
    indice<-!grafo[,1]==grafo[,2]
    indices<-which(mapa!=0,arr.ind = T)    
    grafo<-simplify(graph.data.frame(grafo[indice,],directed=F,vertices=paste("(",indices[,"row"],",",indices[,"col"],")",sep="")))  
    return(grafo)
}

##Função para fazer uma imagem da matriz como vemos ela na tela
plota_mapa<-function(mapa){
    image(t(mapa)[,ncol(mapa):1],col=0:1)
    }


#####################################
## Mapas aleatorios
#####################################
set.seed(123)

##Gera um mapa com threshold de 0.5
mapa<-gera_mapa(10,0.5)
mapa
#jpeg("01.jpg")
plota_mapa(mapa)
#dev.off()

##Threshold de 0.2
mapa<-gera_mapa(10,0.2)
#jpeg("02.jpg")
plota_mapa(mapa)
#dev.off()

##Transformando o mapa em grafo
grafo<-matriz_para_grafo(mapa)

#jpeg("03.jpg")
plot(grafo,vertex.size=20)
#dev.off()

##Usando a função do igraph
components(grafo)

##Realizando uma simulção com 10 repetições para probabilidades de 0.1 a 0.9
repeticoes<-10
probs<-seq(0.1,0.9,0.05)
tamanho<-15

##Matrizes para receber os resultados da simulação
n_compomentes<-matrix(0,ncol=repeticoes,nrow=length(probs),dimnames = list(paste("p=",probs,sep=""),paste("Amostra",1:10)))
maior_componente<-matrix(0,ncol=repeticoes,nrow=length(probs),dimnames = list(paste("p=",probs,sep=""),paste("Amostra",1:10)))

##Realizando a simulação
for(i in 1:length(probs)){
    for(j in 1:repeticoes){
        mapa<-gera_mapa(tamanho,probs[i])
        grafo<-matriz_para_grafo(mapa)
        n_compomentes[i,j]<-components(grafo)$no
        maior_componente[i,j]<-max(components(grafo)$csize)
        print(paste("Concluido i=",i," j=",j,sep=""))
    }
}

##Avaliando o resultado
#jpeg("04.jpg")
par(mfrow=c(2,1))
plot(probs,rowMeans(n_compomentes),type="b",xlim=c(0,1),ylab="Número médio de componentes",xlab="Threshold geração mapa",frame=F)
plot(probs,rowMeans(maior_componente),type="b",xlim=c(0,1),ylab="Tamanho do maior componente",xlab="Threshold geração mapa",frame=F)
#dev.off()
