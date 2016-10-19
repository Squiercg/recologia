#!/usr/bin/env python
'''

'''


def conta_bases(sequencia):	
	contagem={'A':0,'C':0,'G':0,'T':0}
	for base in sequencia:
		contagem[base] = contagem[base] + 1
	return contagem

arquivo = open("/home/augusto/Downloads/rosalind_ini.txt")

sequencia=""


for linha in arquivo:
	sequencia=sequencia+linha.rstrip()

saida = conta_bases(sequencia)

print str(saida['A']) + " " + str(saida['C']) + " " + str(saida['G']) + " " + str(saida['T']) 