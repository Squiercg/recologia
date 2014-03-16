def produto(l):
    """Calcula o produto dos itens de um iterable"""
    t = 1
    for i in l:
        t *= int(i)
    return t


def tripleto(n):
    """Encontra o tripleto pitagorico qual a soma é igual ao numero de entrada"""
    for c in range(n - 3, 1, -1):
        for b in range(c - 1, 1, -1):
            a = (c**2 - b**2)**.5
            if a + b + c == n:
                return [c, b, int(a)]
    return False

print tripleto(1000)
print produto(tripleto(1000))


