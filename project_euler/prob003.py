def primo(n):
    maiorn = n ** 0.5
    i=5
    if n % 2 == 0 and n != 2:
        return False
    if n % 3 == 0 and n != 3:
        return False
    while i < maiorn:
        if n % i == 0:
            return False
        i += 2
    return True

def fatorprimo(n):
    fatores=[]
    i=2
    while True:
        while True:
            if n % i == 0:
                fatores.append(i)
                n = n / i
            else:
                if n == 1:
                    return fatores
                else:
                    break
        while True:
            i = i + 1
            if primo(i):
                break

resposta = fatorprimo(600851475143)

print "Resposta:" , resposta[-1]

produto = 1
for i in resposta:
    produto *= i

print "Multiplicando os fatores para conferir:" , produto
