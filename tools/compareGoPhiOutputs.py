## compare output of two versions of gophi input
##  read old and new and output differences
import sys

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
        print("----")

def getInGroup(label,group):
    for line in group:
        if line.startswith(label):
            return line[len(label):]
    print("%s not found in\n%s"%(label,"\n".join(group)))
    sys.exit(1)

def getAMR(group):
    return "\n".join([line for line in group if not line.startswith("#")])


def compare(oldFileName,newFileName):
    oldF = open(oldFileName,"r",encoding="utf-8")
    newF = open(newFileName,"r",encoding="utf-8")
    print("old:%s"%oldF.readline().rstrip())
    print("new:%s\n"%newF.readline().rstrip())

    oldGroups=groups(oldF)
    newGroups=groups(newF)

    if len(oldGroups)!=len(newGroups):
        print("bad group length:%d::%d"%(len(oldGroups),len(newGroups)))
        sys.exit(1)

    nbDiff=0
    for (oldG,newG) in zip(oldGroups,newGroups):
        oldId=getInGroup("# ::id ",oldG)
        newId=getInGroup("# ::id ",newG)
        if (oldId != newId):
            print("id mismatch:%s::%s\n%s\n%s"%(oldId,newId,"\n".join(oldG),"\n".join(newG)))
            sys.exit(1)
        oldGophi=getInGroup("# ::gophi ",oldG)
        newGophi=getInGroup("# ::gophi ",newG)
        if oldGophi!=newGophi:
            print("snt: %s\nold: %s\nnew: %s\n%s"%(getInGroup("# ::snt ",newG),oldGophi,newGophi,getAMR(newG)))
            print("===")
            nbDiff+=1
    print("%d differences found"%nbDiff)

if __name__ == '__main__':
    oldFileName="/Users/lapalme/Dropbox/PourOrigene/AMR/amr-examples/amr-dict-examples-8.out"
    newFileName="/Users/lapalme/Dropbox/PourOrigene/AMR/amr-examples/amr-dict-examples.out"
    if len(sys.argv)==3:
        oldFileName=sys.argv[1]
        newFileName=sys.argv[2]
    compare(oldFileName,newFileName)

    
