###http://recologia.com.br/2016/05/metodo-de-monte-carlo/

direct_pi<- function(n=4000) {
    n_acertos<-0
    for(i in 1:n){
        x<-runif(n=1,min=-1,max=1)
        y<-runif(n=1,min=-1,max=1)
        if(x^2 + y^2 < 1){
            n_acertos<-n_acertos+1
        }
    }
    return(n_acertos)
}

direct_pi()

repeticoes<-replicate(100,direct_pi())
mean(4*(repeticoes/4000))
pi<max(4*(repeticoes/4000))
pi>min(4*(repeticoes/4000))

#
markov_pi<-function(n=4000,passo=0.1){
    n_acertos<-0
    x<-1
    y<-1
    for(i in 1:n){
        delta_x<-runif(n=1,min=-passo,max=passo)
        delta_y<-runif(n=1,min=-passo,max=passo)
        if(abs(x+delta_x)<1 & abs(y+delta_y)<1){
            x<-x+delta_x
            y<-y+delta_y
        }
        if(x^2 + y^2 < 1){
            n_acertos<-n_acertos+1
        }
    }
    return(n_acertos)
}



markov_pi()

repeticoes<-replicate(100,markov_pi())
mean(4*(repeticoes/4000))
pi<max(4*(repeticoes/4000))
pi>min(4*(repeticoes/4000))


repeticoes<-replicate(100,markov_pi(passo=0.5))
mean(4*(repeticoes/4000))

repeticoes<-replicate(100,markov_pi(passo=0.01))
mean(4*(repeticoes/4000))
pi<max(4*(repeticoes/4000))
pi>min(4*(repeticoes/4000))
