##http://recologia.com.br/2016/06/algebra-de-matrizes-a-matriz-inversa/
##Exemplo
x <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y <- matrix(c(6,2,1),3,1)
x
y


##Matriz identidade
diag(3)

##Matriz inversa de x
solve(x)

##Matriz multiplicado pela matriz inversa
solve(x)%*%x
round(solve(x)%*%x)

##Solução do sistema de equações
solve(x)%*%y
solve(x,y)

##Expandir matriz
p <- nrow(x)
expandido <- cbind(x,y)

##Transformando para uma matriz triangular
expandido[1,] <- expandido[1,]/expandido[1,1]

##A partir da segunda linha
i <- 2
while (i <= p) {
    j <- i
    ## para todas as linhas diminua depois dela da linha de cima, multiplicando pelo elemento da coluna anterior, veja
    ##que toda vez que a gente faz isso, a gente zera uma coluna, da linha 2 para a frente, ou seja, a gente ta transformando
    ##na matriz triangular
    while (j <= p) {
        expandido[j, ] <- expandido[j, ] - expandido[i-1, ] * expandido[j, i-1]
        j <- j+1
    }
    ##depois disso a gente troca as linhas de lugar
    while (expandido[i,i] == 0) {
        expandido <- rbind(expandido[-i,],expandido[i,])
    }
    ##e multiplica por aquel 1/a da formula
    expandido[i,] <- expandido[i,]/expandido[i,i]
    i <- i+1
    ##veja que a cada iteração aqui, uma coluna é zerada, e a gente passa a operar uma submatriz abaixo, ou seja, na
    ##primeira rodada da gente opera da linha 2 pra baixo, na segunda iteração, a da linha 2 pra baixo
}
expandido


##Resolvendo o sistema
for (i in p:2){
    for (j in i:2-1) {
        print(expandido)
        expandido[j, ] <- expandido[j, ] - expandido[i, ] * expandido[j, i]
        
    }
}
expandido


x <- matrix(c(1,1,1,1,1,1,1,1,1),3,3)
y <- matrix(c(1,2,3),3,1)
solve(x,y)
