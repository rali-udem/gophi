#!/usr/local/bin/python3
# coding=utf-8

### generate a sample AMR file for gophi, isiMT and jamr from an AMR file
### can be called as:
###     ./createEvalFiles [fullPath_of_AMR_file] [sampleSize]
###   by default sampleSize=25
###              AMR_file=/Users/lapalme/Dropbox/PourOrigene/AMR/amr-ISI/amr-examples.txt

import re,random,os.path,sys

################################################################################################
#### tokenizer for the SemanticRep input
## tokenizer adapted from https://docs.python.org/3.4/library/re.html#writing-a-tokenizer
currentLine=""

def tokenizeAMR(inputSt):
    global currentLine
    token_specification = [
        # escaped quoted string regex taken from http://stackoverflow.com/questions/16130404/regex-string-and-escaped-quote
        ("STR",           r'"(?:\\.|[^"\\])*?"'+"|"+ r"'(?:\\.|[^'\\])*?'"),# double or single quoted string
        ("NUMBER",        r'-?\d+((\.|:)\d+)?'),         # integer or decimal number or hour:minutes
        ("IDENT",         r'[A-Za-z_][-A-Za-z_0-9]*'),   # Identifiers
        ("ROLE",          r':[A-Za-z_][-A-Za-z_0-9]*'),  # role identifier
        ("OPEN_PAREN",    r'\('),
        ("CLOSE_PAREN",   r'\)'),
        ("SLASH",         r'/'),
        ("BACKSLASH",     r'\\'),
        ("MINUS",         r'-'),
        ("PLUS",          r'\+'),
        ("EOL",           r'\n'),
        ("SKIP",          r'[ \t]+|;.*|#.*'),  # Skip over spaces and tabs and comments 
        ("UNDEF",         r'.')                # Any other character
    ]
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    line_num = 1
    line_start = 0
    line_end=inputSt.find('\n',line_start)
    currentLine=inputSt[line_start:line_end] if line_end>=0 else inputSt[line_start:]
    for mo in re.finditer(tok_regex, inputSt):
        kind = mo.lastgroup
        value = mo.group(kind)
        # print "kind:"+kind+"; value:"+value
        if kind == "EOL":
            line_num += 1
            line_start = mo.end()
            line_end=inputSt.find('\n',line_start)
            currentLine=inputSt[line_start:line_end] if line_end>=0 else inputSt[line_start:]
        elif kind == "SKIP":
            pass
        elif kind == "UNDEF":
            yield Token("UNDEF",value,line_num,mo.start()-line_start)
        else:
            column = mo.start() - line_start
            yield Token(kind, value, line_num, column)
    yield Token("EOF"," ",line_num,0)

class Token:
    """packaging for the output of token"""
    def __init__(self, kind,value,line_num,column):
        self.kind=kind
        self.value=value
        self.line_num=line_num
        self.column=column
        # print self
    
    def __repr__(self):
        return "Token(%s:%s:%d:%d)"%(self.kind,self.value,self.line_num,self.column)

## strip comments and put spaces between tokens to conform to the strict format of JAMR and ISI-MT
def normalizeAMR(lines):
    tokenizer=tokenizeAMR("\n".join(lines))
    token=next(tokenizer)
    outAMR=[]
    while token.kind != "EOF":
        if token.kind in ["IDENT","SLASH","PLUS","MINUS"]:
            outAMR.append(token.value)
            outAMR.append(" ")
        elif token.kind == "ROLE":
            if outAMR[-1]!=" ": outAMR.append(" ")
            outAMR.append(token.value)
            outAMR.append(" ")
        elif token.kind =="CLOSE_PAREN":
            if outAMR[-1]==" ": 
                outAMR[-1]=token.value
            else:
                outAMR.append(token.value)
        elif token.kind=="NUMBER" and ":" in token.value: # for hours...
            outAMR.append('"'+token.value+'"')
        else:
            outAMR.append(token.value)
        token=next(tokenizer)
    return "".join(outAMR)
    
## check if an array of lines has at least a non comment line
def hasAnAMR(lines):
    for line in lines:
        if line[0]!="#":return True
    return False

def groups(file):
    groups=[]
    line=file.readline()
    while len(line)>0:
        lines=[]
        while len(line)>0:
            line=line.rstrip()
            if len(line)==0:
                if len(lines)>0 and hasAnAMR(lines):
                    groups.append(lines)
                    lines=[]
            else:
                lines.append(line)
            line=file.readline()
        if len(lines)>0 and hasAnAMR(lines):
            groups.append(lines)
    return groups

def showGroups(groups):
    for group in groups:
        print(group)
        print(normalizeAMR(group))
        print("----")

def genFiles(fileName,nb):
    file=open(fileName)
    sample=random.sample(groups(file),nb)
    (dirN,fileN)=os.path.split(fileName)
    (root,ext)=os.path.splitext(fileN)
    # generate for gophi
    os.makedirs(os.path.join(dirN,"eval/gophi"),exist_ok=True)
    fileFmt="eval/%s/%s-%d%s"
    gophiFileName=os.path.join(dirN,fileFmt%("gophi",root,nb,ext))
    gophiF=open(gophiFileName,"w",encoding="utf-8")
    for group in sample:
        gophiF.write("\n".join(group)+"\n\n")
    print(gophiFileName+" written")
    # generate for ISI-MT
    os.makedirs(os.path.join(dirN,"eval/isiMT"),exist_ok=True)
    isiMTFileName=os.path.join(dirN,fileFmt%("isiMT",root,nb,ext))
    isiMTF=open(isiMTFileName,"w",encoding="utf-8")
    for group in sample:
        isiMTF.write(normalizeAMR(group)+"\n")
    print(isiMTFileName+" written")
    # generate for JAMR
    os.makedirs(os.path.join(dirN,"eval/jamr"),exist_ok=True)
    jamrFileName=os.path.join(dirN,fileFmt%("jamr",root,nb,ext))
    jamrF=open(jamrFileName,"w",encoding="utf-8")
    for group in sample:
        jamrF.write(normalizeAMR(group)+"\n\n")
    print(jamrFileName+" written")

if __name__ == '__main__':
    fileName="/Users/lapalme/Dropbox/PourOrigene/AMR/amr-ISI/amr-examples.txt"
    if len(sys.argv)>1:
        fileName=sys.argv[1]
    sampleSize=25
    if len(sys.argv)>2:
        sampleSize=int(sys.argv[2])
    genFiles(fileName,sampleSize)