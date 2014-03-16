def palindrome(n):
    return str(n) == str(n)[::-1]

i=999
j=999
numero =[0,0,0]

for i in range(999,99,-1):
    for j in range(999,99,-1):
        if palindrome(i*j) and i*j > numero[0]:
            numero[0] = i*j
            numero[1] = i
            numero[2] = j

print "Resposta:",numero[1],"*",numero[2],"=",numero[0]















