#!/usr/bin/env python3
import sys
for line in open(sys.path[0]+"/englishGenderWords.txt",encoding="utf-8"):
    if line[0]=="#" or len(line)==1:continue
    mots=line.rstrip().split("\t")
    print("gender('%s',\"m\")."%mots[0].strip())
    if len(mots)==2:
        print("gender('%s',\"f\")."%mots[1].strip())

