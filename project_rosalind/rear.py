#!/usr/bin/env python
# coding=utf-8

# Reversal Distance
# =================
# 
# A reversal of a permutation creates a new permutation by inverting some
# interval of the permutation; (5,2,3,1,4), (5,3,4,1,2), and (4,1,2,3,5) are all
# reversals of (5,3,2,1,4). The reversal distance between two permutations π and
# σ, written drev(π,σ), is the minimum number of reversals required to transform
# π into σ (this assumes that π and σ have the same length).
# 
# Given: A collection of at most 5 pairs of permutations, all of which have
# length 10.
# 
# Return: The reversal distance between each permutation pair.
# 
# Sample Dataset
# --------------
# 1 2 3 4 5 6 7 8 9 10
# 3 1 5 2 7 4 9 6 10 8
# 
# 3 10 8 2 5 4 7 1 6 9
# 5 2 3 1 7 4 10 8 6 9
# 
# 8 6 7 9 4 1 3 10 2 5
# 8 2 7 6 9 1 5 3 10 4
# 
# 3 9 10 4 1 8 6 7 5 2
# 2 9 8 5 1 7 3 4 6 10
# 
# 1 2 3 4 5 6 7 8 9 10
# 1 2 3 4 5 6 7 8 9 10
# 
# Sample Output
# -------------
# 9 4 5 7 0


# See the section on inverse permutations and sorting by reversals here:
# http://scholarworks.sjsu.edu/cgi/viewcontent.cgi?article=1103&context=etd_projects

def distancia_reversa(a,b,imprimir=True):
    distancia=0

    if imprimir:
        print "Inicio"
        print a
        print
        print b
        print 
    
    for i in range(len(a),0,-1):
        if a[i-1] != b[i-1]:
            distancia+=1
            copia=list(b)
            inicio=b.index(a[i-1])
            fim=i-1
            for j in range(fim-inicio+1):
                b[inicio+j]=copia[fim-j]
            print b
            print 
    
    if imprimir:
        print "Fim"
        print a
        print b
        print "distancia=",distancia
        print
        print 

    return distancia

if __name__ == "__main__":

    print 'inicio'

    dados= open('/home/augusto-cpcs/Downloads/rosalind_rear.txt')

    resposta=[]

    while True:
        a = dados.readline()
        if not a: break  # EOF
        b = dados.readline()
        a = map(int, a.split(' '))
        b = map(int, b.split(' '))
        resposta.append(distancia_reversa(a,b,True))

    for saida in resposta:
        print saida, 


