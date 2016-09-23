import re
import requests

#arquivo = open('/home/augusto/Downloads/rosalind_mprt.txt','r')
arquivo = open('mprt.in','r')
entradas =  arquivo.read().strip().split('\n')

#print entradas

for entrada in entradas:
    r = requests.get('http://www.uniprot.org/uniprot/%s.fasta' % entrada)
    proteina = r.text
    enter = proteina.find('\n')
    proteina = proteina[(enter+1):]
    proteina = proteina.replace('\n','')
    busca = re.finditer('(?=(N[^P][ST][^P]))',proteina)
    saida=[]
    for i in busca:
        saida.append(i.start()+1)
        #print i.start()+1,
        #print proteina[i.start():i.start()+4]
    if len(saida)>0:
        print entrada
        for i in saida:
            print i,
        print
