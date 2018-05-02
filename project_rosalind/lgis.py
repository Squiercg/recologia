#!/usr/bin/env python
'''
http://recologia.com.br/2018/04/rosalind-longest-increasing-subsequence/
'''

import itertools

def lgis(seq,n):
    maior=[]
    menor=[]
    for i in range(n,2,-1):
        for j in range(0,n):
            for k in itertools.combinations(seq[j:],i):
                k=list(k)
                if len(maior)==0 and sorted(k) == k:
                    maior=k
                if len(menor)==0 and sorted(k,reverse=True) == k:
                    menor=k
                if len(maior)>0 and len(menor)>0:
                    return[menor,maior]

def lgis2(seq,n):
    maior=[]
    menor=[]
    for i in xrange(len(seq), 0, -1):
		for j in itertools.combinations(seq, i):
                    j=list(j)
                    if len(menor)==0 and sorted(j,reverse=True)==j:
                        menor=j
                    if len(maior)==0 and sorted(j)==j:
                        maior=j
                    if len(maior)>0 and len(menor)>0:
                        return [maior,menor]

def lgis3(Sequencia_1,Sequencia_2,imprime_matriz=False):
    m=len(Sequencia_1)
    n=len(Sequencia_2)

    ##Inicializando a matrix dinamica com zeros
    Matriz = [[0 for i in xrange(n+1)] for i in xrange(m+1)]

    #Preenchendo a matrix
    for i in xrange(m+1):
        for j in xrange(n+1):
            if i == 0 or j == 0:
                Matriz[i][j] = 0
            elif Sequencia_1[i-1] == Sequencia_2[j-1]:
                Matriz[i][j] = Matriz[i-1][j-1] + 1
            else:
                Matriz[i][j] = max(Matriz[i-1][j], Matriz[i][j-1])

    if imprime_matriz:
        for i in range(n):
            for j in range(n):
                print Matriz[i][j],
            print 
 
    #Tamanho da maior subsequencia
    tamanho = Matriz[m][n]
 
    #Criando uma lista agora para o traceback
    saida =[]
 
    #Comecando do canto inferior direito da matriz
    i = m
    j = n
    while i > 0 and j > 0:
        # Se nessa posicao as sequencias sao iguais, so adicionar ela na menor subsequencia
        if Sequencia_1[i-1] == Sequencia_2[j-1]:
            saida.append(Sequencia_1[i-1])
            i-=1
            j-=1
            tamanho-=1
        else:
            #senao temos que olhar na matriz a proxima posicao que elas sao iguais
            if Matriz[i-1][j] > Matriz[i][j-1]:
                #ou andamos linha acima
                i-=1
            else:
                #ou coluna para esquerda
                j-=1
 
    return saida[::-1]
		    
                    

if __name__ == '__main__':
    
    n = 5
    seq = [5,1,4,2,3]

    '''
    file = open( "/mnt/B21AA1BD1AA17ECB/downloads_chromium_ubuntu/rosalind_lgis.txt" )
    n=int(file.readline())
    seq=map(int, file.readline().split())
    file.close()
    '''   

    #print "n="+str(n)
    #print seq

    #lista=lgis(seq,n)
    #lista=lgis2(seq,n)

    a=lgis3(seq,sorted(seq))
    b=lgis3(seq,sorted(seq,reverse=True))

    '''
    file = open('resposta.txt','w')
    for i in xrange(len(a)-1):
        file.write(str(a[i])+' ')
    file.write(str(a[len(a)-1]))
    file.write('\n')
    for i in xrange(len(b)-1):
        file.write(str(b[i])+' ')
    file.write(str(b[len(b)-1]))
    '''


    for sequencia in [a,b]:
        for i in sequencia:
            print i,
        print


        
    


        


    

    








