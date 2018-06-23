#

import sys
from Bio import Phylo
import io
from cStringIO import StringIO

arquivo = open('/home/augusto/Downloads/rosalind_nkew.txt','r')

linhas=arquivo.readlines()

print 
for i in range(0,len(linhas),3):
    arvore=Phylo.read(StringIO(linhas[i].strip()),'newick')
    taxas=linhas[i+1].strip().split()
    #Phylo.draw_ascii(arvore)
    print("%.0f" % round(arvore.distance(taxas[0],taxas[1]))),
