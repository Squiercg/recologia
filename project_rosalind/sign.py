import itertools 

n = 3

a = []

for i in range(n):
  a.append(i+1)
#print a

perm = list(itertools.permutations(a))
#print perm

print len(perm) * (2**n)

for p in perm:
  for i in range(2**n):
    for j in range(len(p)):
      if (i>>j) & 1:
        sinal = '-'
      else:
        sinal = ''
      print sinal + str(p[j]),
    print ''    
