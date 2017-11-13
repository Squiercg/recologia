#!/usr/bin/env python

import sys
import random

###Explicacoes no link
###http://recologia.com.br/2017/11/random-web-surfer-como-o-google-classifica-suas-paginas/

class Randomwebsurfer(object):
    n=None
    matriz=None
    grau=None
    transisao=None
    frequencia=None

    def __init__(self,n):
        self.n=n
        self.matriz=self.inicia_matriz(n)


    def simulacao(self,passos):
        '''
        Esse metodo realiza a simulacao de navegacao por um usuario.

        '''
        self.inicia_matriz_transisao()

        if self.frequencia==None:
            self.frequencia=[0]*self.n

        pagina=0
        i=0
        while i < passos:
            sorteio=random.random()
            soma=0.0

            for j in xrange(self.n):
                soma+=self.transisao[pagina][j]
                if sorteio < soma:
                    pagina=j
                    break

            self.frequencia[pagina]+=1
            i+=1
            

    def inicia_matriz(self,n):
        '''
        Esse metodo inicia a matriz que representa a rede, mas sem nenhuma conecao entre paginas,
        recebe um argumento que e o numero de paginas do sistema

        '''
        matriz = [[0 for i in xrange(n)] for j in xrange(n)]
        return matriz

    def adiciona_link(self,linha,coluna):
        '''
        Esse metodo adiciona uma conecao entre duas paginas, sendo o primeiro argumento a origem e o segundo o destino

        '''
        self.matriz[linha][coluna]+=1

    def calcula_grau(self):
        '''
        Esse metodo calcula o grau de cada pagina.

        '''
        self.grau=[0]*self.n
        
        for i in range(self.n):
            for j in range(self.n):
                self.grau[i]+=self.matriz[i][j]

    def inicia_matriz_transisao(self):
        '''
        Esse metodo calcula a matriz de transicao, a matriz e links precisam estar todos adicionados

        '''
        self.calcula_grau()        
        pulo=[[0 for i in xrange(n)] for j in xrange(n)]
        link=[[0 for i in xrange(n)] for j in xrange(n)]
        self.transisao=[[0 for i in xrange(n)] for j in xrange(n)]

        for i in range(self.n):
            for j in range(self.n):
                pulo[i][j]=0.1/float(self.n)

        for i in range(self.n):
            for j in range(self.n):
                link[i][j]=self.matriz[i][j] * (0.9/float(self.grau[i]))

        for i in range(self.n):
            for j in range(self.n):
                self.transisao[i][j]=pulo[i][j]+link[i][j]       
                
    def imprime_matriz(self):
        '''
        Esse metodo imprime a matriz de dados que presenta o sistema

        '''
        for linha in self.matriz:
            print ''
            for item in linha:
                print item,
        print ''

    def imprime_resultado_simulacao(self):
        '''
        Esse metodo imprime o resultado da simulacao

        '''
        for i in self.frequencia:
            print i, 

    


#####################################
## Random Surfer
#####################################
        
if __name__ == '__main__':
    
    print "Lendo entrada..."
    arquivo = open(sys.argv[1], 'r')
    n=int(arquivo.readline())
    print "Numero de paginas: " +  str(n)

    minharede=Randomwebsurfer(n)

    for linha in arquivo:
        vetor=linha.strip('\n')
        vetor=vetor.split(' ')        
        while '' in vetor: vetor.remove('')

        for i in range(0,len(vetor),2):
            minharede.adiciona_link(int(vetor[i]),int(vetor[i+1]))


    print "Matriz de entrada:",
    minharede.imprime_matriz()

    print ''
    print 'Rodando simulacao...'
    minharede.simulacao(100)

    print ''
    print 'Resultado:'
    minharede.imprime_resultado_simulacao()

        
