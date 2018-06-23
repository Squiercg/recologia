n <- 220
dividendo <- n
divisor <- 1:(n-1)
vetor <- dividendo/divisor  
sum(divisor[!vetor%%1])

##Função
sdp <- function(n){
    dividendo <- n
    divisor <- 1:(n/2)
    vetor <- dividendo/divisor  
    saida <- sum(divisor[!vetor%%1])
    return(saida)
}


##teste
a <- 220
b <- sdp(a)
a==sdp(b) && b==sdp(a)

a <- 100
b <- sdp(a)
a==sdp(b) && b==sdp(a)

#solução
soma <- 0
for( a in 2:9999){
    b <- sdp(a)
    if(b>a){
        if(sdp(b)==a){
            soma <- soma+a+b
        }
    }
}
soma
