##http://recologia.com.br/2016/06/provas-matematicas/
primo <- function(numero) {
   if (numero == 2) {
      TRUE
   } else if (any(numero %% 2:(numero-1) == 0)) {
      FALSE
   } else { 
      TRUE
   }
}

###
exemplo<-data.frame(numero=2:10)
exemplo$primo<-sapply(exemplo$numero,primo)
exemplo

###
exemplo$pot<-(2^exemplo$numero)-1
exemplo$primo_pot<-sapply(exemplo$pot,primo)
exemplo

###
exemplo<-data.frame(numero=2:11)
exemplo$primo<-sapply(exemplo$numero,primo)
exemplo$pot<-(2^exemplo$numero)-1
exemplo$primo_pot<-sapply(exemplo$pot,primo)
exemplo
