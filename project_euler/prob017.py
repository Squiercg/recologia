p= {'1-9': ['one',
         'two',
         'three',
         'four',
         'five',
         'six',
         'seven',
         'eight',
         'nine'],
 '10-19': ['ten',
           'eleven',
           'twelve',
           'thirteen',
           'fourteen',
           'fifteen',
           'sixteen',
           'seventeen',
           'eighteen',
           'nineteen'],
 '20-90': ['twenty',
           'thirty',
           'forty',
           'fifty',
           'sixty',
           'seventy',
           'eighty',
           'ninety'],
 'x00': 'hundred'}

def hundred(num):
    """ num is from 0-9 """
    pfx = 0     # prefix applied to each number processed
    l = 0       # overall length
    if num > 0:
        pfx = len(p['1-9'][num - 1]) + len(p['x00'])
        l += pfx # simulate counting the first, i.e. one hundred
        pfx += 3 # add 3 to prefix, for xxx hundred `AND`
        l += pfx * 99 # apply prefix to all the numbers
    l += sum([len(x) for x in p['1-9']]) # the good
    l += sum([len(x) for x in p['10-19']]) # the bad
    l += sum([len(x) + sum([len(y) + len(x) for y in p['1-9']]) for x in p['20-90']]) # the ugly
    return l

print sum([hundred(x) for x in range(0, 10)]) + 11 # 11 is length of 'onethousand'