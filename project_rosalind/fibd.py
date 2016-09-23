

def reverse_complement(s):
    complements = {'A':'T', 'T':'A', 'G':'C', 'C':'G'}
    return "".join([complements[c] for c in reversed(s)])


def reverse_palindromes(s):
    results = []

    l = len(s)

    for i in range(l):
        for j in range(4, 12):

            if i + j > l:
                continue

            s1 = s[i:i+j]
            s2 = reverse_complement(s1)

            if s1 == s2:
                results.append((i + 1, j))

    return results

teste = "TCAATGCATGCGGGTCTATATGCAT"

teste2 = reverse_complement(teste)

print teste[4:10]
print teste2[10:4]




 
