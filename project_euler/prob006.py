numero1 = []
numero2 = 0

for i in range(1,101):
    numero1.append(i*i)

for i in range(1,101):
    numero2 = numero2 + i

numero2=numero2*numero2


print  numero2-sum(numero1)



