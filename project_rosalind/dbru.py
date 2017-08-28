def construct_debruin_graph(reads):
    graph = {}
    for read in reads:
        a, b = read[:-1], read[1:]
        if a not in graph:
            graph[a] = set()
        if b not in graph:
            graph[b] = set()
        graph[a].add(b)
    return graph

def reverse_complement(read):
    mapping = {
        "A" : "T",
        "C" : "G",
        "G" : "C",
        "T" : "A"
    }
    return "".join(map(lambda x : mapping[x], list(read[::-1])))

'''
f = open("/home/augusto/downloads_chromium_ubuntu/rosalind_dbru.txt",'r')
reads = []
for line in f:
    reads.append(line.strip())
'''

reads=['TGAT','CATG','TCAT','ATGC','CATC','CATC']

graph = construct_debruin_graph(reads + map(reverse_complement, reads))
for node in graph:
    if len(graph[node]) > 0:
        for neighbour in graph[node]:
            print "({a}, {b})".format(a = node, b = neighbour)

