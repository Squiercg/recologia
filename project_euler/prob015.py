# combination
# I solved using (very ugly) brute force to generate the first 5 terms
# in the sequence, which I then entered into Wolfram Alpha.
# http://mathworld.wolfram.com/BinomialCoefficient.html

# the brute force solution is at the bottom
def fac(n):
    if n > 1:
        return n * fac(n - 1)
    else:
        return 1

# binomial
print fac(40)/(fac(20)**2)


# brute force (it's slow).  guess what i'm doing:
import string

def s2b (s):
    n = int(s)
    c = ''
    while n > 0:
        c = str(n % 2) + c
        n /= 2
    return c
    
def b2s (b):
    return string.atoi(b, 2)

def max_paths(length_of_side):
    i = 0
    c = 0
    maximum = ''
    for j in range(0, length_of_side):
        maximum = '1' + maximum + '0'
    maximum = b2s(maximum)
    while i <= maximum:
        b = s2b(i)
        b = b.zfill(2 * length_of_side)
        if b.count('0') == b.count('1'):
            c += 1
        i += 1
    return c

print "First 5 terms in sequence:"
for i in range(1, 6):
    print max_paths(i)