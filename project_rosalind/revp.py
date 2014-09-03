def reverse_complement(s):
    complements = {'A':'T', 'T':'A', 'G':'C', 'C':'G'}
    return ''.join([complements[c] for c in reversed(s)])


def reverse_palindromes(s):
    results = []

    l = len(s)

    for i in range(l):
        for j in range(4, 9):

            if i + j > l:
                continue

            s1 = s[i:i+j]
            s2 = reverse_complement(s1)

            if s1 == s2:
                results.append((i + 1, j))

    return results


if __name__ == "__main__":

    small_dataset = "TCAATGCATGCGGGTCTATATGCAT"
    large_dataset = open('/home/augusto/Downloads/rosalind_revp.txt').read().strip()

    results = reverse_palindromes(large_dataset)

    print "\n".join([' '.join(map(str, r)) for r in results])