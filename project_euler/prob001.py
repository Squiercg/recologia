#If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
#The sum of these multiples is 23.
#Find the sum of all the multiples of 3 or 5 below 1000.

menor = 0
maior = 1000
passo = 1
soma = 0

while menor < maior:
    if menor % 3 == 0 or menor % 5 == 0:
        soma += menor
    menor += passo

print soma
