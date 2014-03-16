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


soma = 2
for i in range(3, 2000000, 2):
    if primo(i):
        soma += i

print soma


