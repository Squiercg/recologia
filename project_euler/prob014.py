import time
# SAMPLE SIZE   10000
#               WITHOUT CACHING:    WITH CACHING:
# time:         1.51473093033       0.30783700943

# SAMPLE SIZE   100000
#               WITHOUT CACHING:    WITH CACHING:
# time:         19.0975861549       3.01840496063

# SAMPLE SIZE   1000000
#               WITHOUT CACHING:    WITH CACHING:
# time:         ???                 32.3304488659

# note that caching results in roughly linear performance
# while without

# I'm running these on a Pentium III@800MHz
# IBM Thinkpad T21 (ohhh yeah)
def seq(n):
    if n % 2 == 0:
        return n / 2
    else:
        return (3*n) + 1

num = maximum = 0

start = time.time()

cache = { 1:1 }

for i in range(1, 1000000, 2):
    result = i
    num_terms = 0
    while result != 1:
        if cache.has_key(result):
            num_terms += cache[result]
            break
        else:
            num_terms += 1
            result = seq(result)
    cache[i] = num_terms
    if num_terms > maximum:
        num = i
        maximum = num_terms

print num

end = time.time()
print end - start