#Post:
#http://recologia.com.br/2015/07/rosalind-locating-restriction-sites/

def complemento_reverso(s):
    complementos = {'A':'T', 'T':'A', 'G':'C', 'C':'G'}
    return ''.join([complementos[c] for c in reversed(s)])

def palindromo_reverso(s):
    saida = []

    l = len(s)

    for i in range(0,l-4):
        for j in range(4, 13):
            if i + j < l:
                s1 = s[i:i+j]
                s2 = complemento_reverso(s1)

                if s1 == s2:
                    #print "s1:",s1," s2:",s2
                    saida.append([i + 1, j])

    return saida

##################################
## Processamento
##################################

sequencia = "TCAATGCATGCGGGTCTATATGCAT"
#dados = open('/home/augusto/Downloads/rosalind_revp.txt').read().strip()
#dados = dados[dados.find('\n')+1:].replace('\n','')

saida = palindromo_reverso(sequencia)

print
for i in saida:
    print i[0],i[1]
