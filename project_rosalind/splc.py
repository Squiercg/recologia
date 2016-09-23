#!/usr/bin/env python
'''

'''

import re

RNA_CODON_TABLE = {
'ATA':'I', 'ATC':'I', 'ATT':'I', 'ATG':'M',
'ACA':'T', 'ACC':'T', 'ACG':'T', 'ACT':'T',
'AAC':'N', 'AAT':'N', 'AAA':'K', 'AAG':'K',
'AGC':'S', 'AGT':'S', 'AGA':'R', 'AGG':'R',
'CTA':'L', 'CTC':'L', 'CTG':'L', 'CTT':'L',
'CCA':'P', 'CCC':'P', 'CCG':'P', 'CCT':'P',
'CAC':'H', 'CAT':'H', 'CAA':'Q', 'CAG':'Q',
'CGA':'R', 'CGC':'R', 'CGG':'R', 'CGT':'R',
'GTA':'V', 'GTC':'V', 'GTG':'V', 'GTT':'V',
'GCA':'A', 'GCC':'A', 'GCG':'A', 'GCT':'A',
'GAC':'D', 'GAT':'D', 'GAA':'E', 'GAG':'E',
'GGA':'G', 'GGC':'G', 'GGG':'G', 'GGT':'G',
'TCA':'S', 'TCC':'S', 'TCG':'S', 'TCT':'S',
'TTC':'F', 'TTT':'F', 'TTA':'L', 'TTG':'L',
'TAC':'Y', 'TAT':'Y', 'TAA':'_', 'TAG':'_',
'TGC':'C', 'TGT':'C', 'TGA':'_', 'TGG':'W',
}
 
 
def protein_string(mrna):
    result = ''
 
    for i in range(0, len(mrna), 3):
        symbol = RNA_CODON_TABLE[mrna[i:i+3]]
        if symbol == '_':
            break
        result += symbol
 
    return result



arquivo = open("/home/augusto/Downloads/rosalind_splc.txt")

sequencia=[]
i=-1

for linha in arquivo:
	if linha.find('>') != -1:		
		i+=1
		sequencia.append('')
	else:		
		sequencia[i]=sequencia[i]+linha.rstrip()


principal=sequencia[0]
sequencia.pop(0)


for seq in sequencia:
	principal=re.sub(seq, '', principal)

print protein_string(principal)