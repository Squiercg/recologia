#!/usr/bin/env python
'''

'''
from Bio import Entrez
from Bio import SeqIO

arquivo = open('/home/augusto/Downloads/rosalind_need.txt')

ids=[]
for linha in arquivo:
	ids.extend(linha.rstrip().split())

Entrez.email = 'ribas.aca@gmail.com'
handle = Entrez.efetch(db='nucleotide', id=ids, rettype='fasta')
records = list(SeqIO.parse(handle, 'fasta'))


sequencia=records[0].seq
lista=[]
while sequencia:
    lista.append(sequencia[:70])
    sequencia = sequencia[70:]

print '>'+records[0].description
for linha in lista:
	print linha

sequencia=records[1].seq
lista=[]
while sequencia:
    lista.append(sequencia[:70])
    sequencia = sequencia[70:]

print '>'+records[1].description
for linha in lista:
	print linha