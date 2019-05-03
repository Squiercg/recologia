## funcao para calcular a distancia entre duas sequencias
def hamming(seq1, seq2):
    mutacoes = 0
    for i in range(len(seq1)):
        if seq1[i] != seq2[i]:
            mutacoes += 1
    return mutacoes

## funcao para detectar quem e erro de sequenciamento, so ocorre uma vez como read
def separa_erros(contagens, sequencias_originais):
    reads = []
    erros = []
    for s in contagens:
        if contagens[s] >= 2:
            reads.append(s)
        elif s in sequencias_originais:
            erros.append(s)
    return reads, erros

## funcao para corrigir erros de sequenciamento
def corrigir(reads, erros):
    corrigido = []
    for erro in erros:
        for read in reads:
            if hamming(erro, read) == 1:
                corrigido.append([erro, read])
    return corrigido

if __name__ == "__main__":
    import Bio.SeqIO    
    import collections 

    ## abrir arquivo fasta com sequencias
    arquivo = open("/home/augusto-cpcs/Downloads/rosalind_corr.txt", 'r')
    sequencias = []
    sequencias_originais = []
    for s in Bio.SeqIO.parse(arquivo, "fasta"):
        sequencias_originais.append(str(s.seq))
        sequencias.append(str(s.seq))
        sequencias.append(str(s.seq.reverse_complement()))
    arquivo.close()

    ##contar as sequencias, corretas aparecem pelo menos 2 vezes
    contagens = collections.Counter(sequencias)

    print "Reads e suas contagens:"
    for i in range(0,len(contagens)):
        print contagens.items()[i]
    print

    
    reads, erros = separa_erros(contagens, sequencias_originais)
    correcoes = corrigir(reads, erros)

    for i in correcoes:
        print "->".join(i)

