from string import maketrans

DNA_CODON_TABLE = {
    'TTT': 'F',     'CTT': 'L',     'ATT': 'I',     'GTT': 'V',
    'TTC': 'F',     'CTC': 'L',     'ATC': 'I',     'GTC': 'V',
    'TTA': 'L',     'CTA': 'L',     'ATA': 'I',     'GTA': 'V',
    'TTG': 'L',     'CTG': 'L',     'ATG': 'M',     'GTG': 'V',
    'TCT': 'S',     'CCT': 'P',     'ACT': 'T',     'GCT': 'A',
    'TCC': 'S',     'CCC': 'P',     'ACC': 'T',     'GCC': 'A',
    'TCA': 'S',     'CCA': 'P',     'ACA': 'T',     'GCA': 'A',
    'TCG': 'S',     'CCG': 'P',     'ACG': 'T',     'GCG': 'A',
    'TAT': 'Y',     'CAT': 'H',     'AAT': 'N',     'GAT': 'D',
    'TAC': 'Y',     'CAC': 'H',     'AAC': 'N',     'GAC': 'D',
    'TAA': 'Stop',  'CAA': 'Q',     'AAA': 'K',     'GAA': 'E',
    'TAG': 'Stop',  'CAG': 'Q',     'AAG': 'K',     'GAG': 'E',
    'TGT': 'C',     'CGT': 'R',     'AGT': 'S',     'GGT': 'G',
    'TGC': 'C',     'CGC': 'R',     'AGC': 'S',     'GGC': 'G',
    'TGA': 'Stop',  'CGA': 'R',     'AGA': 'R',     'GGA': 'G',
    'TGG': 'W',     'CGG': 'R',     'AGG': 'R',     'GGG': 'G'
}

def revc(sequencia):
    intab = "ACTG"
    outtab = "TGAC"
    trantab = maketrans(intab, outtab)
    return(sequencia.translate(trantab)[::-1])

def traduzir_codon(codon):
    amino = ""
    if len(codon) == 3 and DNA_CODON_TABLE.has_key(codon):
        amino = DNA_CODON_TABLE[codon]
    return amino

def dna2prot(seqdna,inicio):
    result = ""
    for i in range(inicio, len(seqdna), 3):
        aminoacido = traduzir_codon(seqdna[i:i+3])
        if aminoacido == 'Stop':
            return result
        else:
            result += aminoacido
    return ""

def possibilidades(sequencia):
    possibilidades = []
    inicio = sequencia.find("ATG",0)
    while inicio != -1:
        resul = dna2prot(sequencia,inicio)
        inicio = sequencia.find("ATG",inicio+1)
        if resul != "":
            possibilidades.append(resul)
    return possibilidades

exemplo = "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"
arquivo = open('/home/augusto/Downloads/rosalind_orf.txt')

dados = arquivo.readline()
dataset=""

for line in arquivo:
        dataset+=line.strip()

#print str(dataset)


A = possibilidades(exemplo)
B = possibilidades(revc(exemplo))

print "\n".join(set(A + B))













