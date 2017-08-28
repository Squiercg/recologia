def combina2(alfabeto,n,kmer,lista):
    if n>0:
        for letra in alfabeto:
            lista.append(kmer+letra)
            combina2(alfabeto,n-1,kmer+letra,lista)
            
    return lista


file = open( "/mnt/B21AA1BD1AA17ECB/downloads_chromium_ubuntu/rosalind_lexv.txt" )
alfabeto = file.readline().split()
n = int(file.readline())
file.close()


###Exemplo
###alfabeto=['D','N','A']
###n=3

resposta= combina2(alfabeto,n,'',[])

for i in resposta:
    print i



    


