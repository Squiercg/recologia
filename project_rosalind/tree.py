def minarv(n, edges):
    return n - len(edges) - 1

n=10
edges=[(1, 2), (2, 8), (4, 10), (5, 9), (6, 10), (7, 9)]

print minarv(n,edges)

file = open( "/home/augusto/Downloads/rosalind_tree.txt" )
n = file.readline()
edges = []

for linha in file:
    edges.append(linha.split())

file.close()

print int(n)
print len(edges)

print minarv(int(n),edges)