from numpy import prod

arquivo = open('prob008.txt', 'r')

lista=[]
for linha in arquivo:
     lista.append(linha.split())

arquivo.close()

valores=[]
for linha in range(0,len(lista)):
    for numero in str(lista[linha])[2:52]:
        valores.append(int(numero))

produto=0
for i in range(0,len(valores)-5):
    if produto<prod(valores[i:i+5]):
        produto=prod(valores[i:i+5])

print produto


