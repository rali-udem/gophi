#!/usr/bin/env python
# coding=utf-8

### filtre Unix pour indenter un AMR
##   en tenant compte du niveau des parenthèses, on début chaque role sur une nouvelle ligne
## les lignes débutant par un commentaires sont laissées telles quelles
## ATTENTION: on ne traite pas le cas de deux-points ou parenthèses dans une chaîne
import sys,re

indent="    "
level=0
for line in sys.stdin:
    l=line.strip()
    if (l.startswith("#")):
        sys.stdout.write(line)
    else:
        for l2 in re.findall(r'(?:^|:)[^:]+',l): # separer la ligne aux :
            sys.stdout.write(level*indent+l2+"\n")
            for c in l2:
                if c=="(":level+=1
                if c==")":level-=1

    