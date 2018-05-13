#!/usr/bin/env python
'''
http://recologia.com.br/2016/10/rosalind-suboptimal-local-alignment/
'''

def hamming_dist(a,b,limite_erros=5):
    erros = 0
    for i in range(len(a)):
        if a[i] != b[i]:
            erros += 1
            if erros>limite_erros:
            	return erros
    return erros 


arquivo = open("/home/augusto/Downloads/rosalind_subo.txt")

sequencia=[]
i=-1

for linha in arquivo:
	if linha.find('>') != -1:		
		i+=1
		sequencia.append('')
	else:		
		sequencia[i]=sequencia[i]+linha.rstrip()


def subo(a, b):
	contagem_final=0
	for tamanho in range(32,41):
		for i in range(len(a)-tamanho):
			contagem=0
			for j in range(len(b)-tamanho):
				if hamming_dist(a[i:i+tamanho], b[j:j+tamanho])<=3:
					contagem+=1
					print a[i:i+tamanho]
					if contagem>contagem_final:
						contagem_final=contagem
	return contagem_final
print subo(sequencia[1], sequencia[0])
print subo(sequencia[0], sequencia[1])
