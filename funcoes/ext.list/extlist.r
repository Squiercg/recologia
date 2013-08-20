ext.list<-function(dados,extintos) {
  lista.extintos<-rownames(dados)[rowSums(dados[,!(dimnames(dados)[[2]]%in%extintos),drop=F])<1]
  return(lista.extintos)
  }


extlist<-function(matriz,nsim=50,probabilidades) {

    if(class(matriz)!="data.frame") {
    matriz<-as.data.frame(matriz)
    }

    lista.final<-list()

    print("Fazendo Simulações")

    for (j in 1:nsim) {
        extintosimu<-sample(colnames(matriz),ncol(matriz),replace=F,prob =probabilidades)
        lista.aux<-list()
        for(i in 1:ncol(matriz)) {
            lista.aux[[i]]<-ext.list(matriz,extintosimu[1:i])
            }
        lista.final[[j]]<-lista.aux
        }
    hist.final<-matrix(NA,ncol=nsim,nrow=nrow(matriz),dimnames=list(rownames(matriz),paste("Simulação",1:nsim,sep=" ")))

    print("Organizndo os dados")

    for(j in rownames(hist.final)) {
        print(paste("sp ",j))
        for(i in 1:nsim) {
            hist.final[j,i]<-which(mapply("%in%",j, lista.final[[i]]))[1]
            }
        }

    return(hist.final)
    }
