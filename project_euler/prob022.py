import string

#Abrir o arquivo
arquivo = open('/home/augusto-cpcs/Downloads/p022_names.txt','r')
linha = arquivo.read()
vetor = linha.split(",")
vetor.sort()
for i in range(len(vetor)):
    vetor[i]=vetor[i].replace('"','')

#Criar o dicionario com valor de cada letra
dicionario={}
valor=1
letras = string.uppercase
for i in letras:
    dicionario[i]=valor
    valor+=1

##Vamos fazer a soma total    
soma_total = 0
##Para cada palavra
for posicao in range(len(vetor)):
    soma_palavra = 0
    ##Para cada letra
    for letra in vetor[posicao]:
        ##Somamos o valor da letra
        soma_palavra += dicionario[letra]
    ##e multiplicamos pela posicao
    soma_total += soma_palavra*(posicao+1)
##E assim temos a resposta.
print soma_total
