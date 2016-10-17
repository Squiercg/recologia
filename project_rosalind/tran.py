#!/usr/bin/env python
'''

'''


def razao_tstv(s1, s2):
    transicoes = set([('A', 'G'), ('G', 'A'), ('C', 'T'), ('T', 'C')])
    razao = {True: 0.0, False: 0.0}
    for p in zip(s1, s2):
        if p[0] != p[1]:
            razao[p in transicoes] += 1
    return razao[True] / razao[False]



arquivo = open("/home/augusto/Downloads/rosalind_tran.txt")

sequencia=[]
i=-1

for linha in arquivo:
	if linha.find('>') != -1:		
		i+=1
		sequencia.append('')
	else:		
		sequencia[i]=sequencia[i]+linha.rstrip()

print razao_tstv(sequencia[0], sequencia[1])