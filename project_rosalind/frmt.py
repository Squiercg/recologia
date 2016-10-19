#!/usr/bin/env python
'''

'''




from Bio import Entrez
from Bio import SeqIO

arquivo = open('/home/augusto/Downloads/rosalind_frmt.txt')

ids=[]
for linha in arquivo:
	ids.extend(linha.rstrip().split())

Entrez.email = 'ribas.aca@gmail.com'
handle = Entrez.efetch(db='nucleotide', id=ids, rettype='fasta')
records = list(SeqIO.parse(handle, 'fasta'))

menor=10**6
indice=-1
for i in range(len(records)):
	if len(records[i].seq) < menor:
		menor=len(records[i].seq)
		indice=i

print indice

sequencia=records[indice].seq
lista=[]
while sequencia:
    lista.append(sequencia[:70])
    sequencia = sequencia[70:]

print '>'+records[indice].description
for linha in lista:
	print linha
