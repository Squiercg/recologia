from math import log10

#with open('/home/augusto/Downloads/rosalind_prob.txt') as input_data:
#    dna, prob_gc = input_data.readlines()

#Exemplo
dna='ACGATACAA'
prob_gc='0.129 0.287 0.423 0.476 0.641 0.742 0.783'
        
prob_gc = map(float, prob_gc.split())

#Conta o número de GC e AT
conteudo_gc = 0
conteudo_at = 0
for codon in dna:
	if codon in ['C', 'G']:
		conteudo_gc += 1
        else:
                conteudo_at += 1



gc=0.129
total=1.0
for base in dna:
        if base in ["G","C"]:
                total*=  gc*0.5
        else:
                total*=  (1-gc)*0.5
                
print log10(total)

total=0.0
for base in dna:
        if base in ["G","C"]:
                total+=  log10(gc*0.5)
        else:
                total+=  log10((1-gc)*0.5)
                
print total
	
prob_lista  = []
for prob in prob_gc:        
	prob_final = conteudo_gc*log10(0.5*prob) + (len(dna)-conteudo_gc) * log10(0.5*(1-prob))
	prob_lista.append(str(prob_final))

print ' '.join(prob_lista)


prob_gc=(float(conteudo_gc)/len(dna))
prob_at=(float(conteudo_at)/len(dna))







        
