#!/usr/bin/python
from sys import argv
from random import random, seed

fin = argv[1]
fout1 = open(argv[2], mode="w", encoding='utf-8')
fout2 = open(argv[3], mode="w", encoding='utf-8')
percent = float(argv[4])
seed = int(argv[5])

for i,line in enumerate(open(fin, encoding='utf-8')):
    if i==0:
        fout1.write(line)
        fout2.write(line)
    else:
        if random()<percent:
            fout1.write(line)
        else:
            fout2.write(line)
            
fout1.close()
fout2.close()
