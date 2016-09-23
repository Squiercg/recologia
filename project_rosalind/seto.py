import sys

def conjuntos(n, a, b):
    return [
        a | b,
        a & b,
        a - b,
        b - a,
        set(range(1, n + 1)) - a,
        set(range(1, n + 1)) - b
    ]


'''
n=10
a=set([1, 2, 3, 4, 5])
b=set([2, 8, 5, 10])

saida=conjuntos(n,a,b)
'''
dados=open("/home/augusto/Downloads/rosalind_seto.txt",'r').read()

n=int(dados[0:dados.find('\n')])

chave1=dados.find('{')
chave2=dados.find('{',chave1+1)

a=dados[chave1+1:dados.find('}')]
a=a.split(', ')
a=map(int,a)
a=set(a)

b=dados[chave2+1:dados.find('}',chave2)]
b=b.split(', ')
b=map(int,b)
b=set(b)

saida=conjuntos(n,a,b)

for conjunto in saida:
    conjunto=list(conjunto)
    tamanho=len(conjunto)
    print "{",
    for i in range(0,tamanho):
        if i == tamanho-1:
            sys.stdout.write("%d" % conjunto[i])
        else:
            sys.stdout.write("%d, " % conjunto[i])
    print"}"
