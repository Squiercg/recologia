def combina(alfabeto,n,kmer,lista):
    if n==0:

        lista[kmer]=0
    else:
        for letra in alfabeto:
            combina(alfabeto,n-1,kmer+letra,lista)
    return lista

def conta_ocorrencia(kmer,sequencia):
    contagem=0
    i=sequencia.find(kmer)
    if i==-1:
        return 0
    else:
        while i!=-1: 
            contagem+=1
            i=sequencia.find(kmer,i+1)
    return contagem
        


file = open( "/mnt/B21AA1BD1AA17ECB/downloads_chromium_ubuntu/rosalind_kmer.txt" )
file.readline()
sequencia=""
for linha in file:
	sequencia=sequencia+linha.rstrip()
file.close()


alfabeto=['A','C','G','T']
n=4

possibilidades=combina(alfabeto,n,'',{})


#sequencia="CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGGCCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGTTTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCAAATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCGGGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGACTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTACCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG"



for i in sorted(possibilidades.keys()):
    print conta_ocorrencia(i,sequencia),




