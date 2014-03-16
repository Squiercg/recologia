#http://pythonism.wordpress.com/2008/05/04/looking-at-prime-numbers-in-python/
def primo(n):
    n*=1.0
    if n % 2 == 0 and n != 2 or n % 3 == 0 and n != 3:
        return False
    for b in range( 1 , int( (n**0.5+1) / 6.0+1) ):
        if n % (6 * b - 1) == 0:
            return False
        if n % (6 * b + 1) == 0:
           return False
    return True


lista = []
i=2
posicao = 10001

while len(lista)<posicao:
    if primo(i):
        lista.append(i)
    i += 1

print lista[-1]
