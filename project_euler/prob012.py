import math # use sqrt to find upper bound of loop

def get_factors(n):
    factors = []
    factors.append(1)
    # loop to square root as any higher factors can be determined from lower factors
    for j in range(2, int(math.sqrt(n)) + 1):
        if n % j == 0:
            factors.append(j) # factor
            factors.append(n/j) # find it's partner
    factors.append(n)
    return set(factors) # sorted unique terms

# Finding the number is now easy:
i = 0
n = 0
factors = []

while len(factors) < 500:
    i += 1
    n += i
    factors = get_factors(n)

print n