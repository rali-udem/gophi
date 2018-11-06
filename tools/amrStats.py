'''
Created on 16 avr. 2018

@author: lapalme
'''
import io,re,datetime,sys

# from levenshtein import compareLevenshtein
from calculatebleu import BLEU

from openpyxl import Workbook
from openpyxl.styles import Alignment,Font

linkPat=r'<a[^>]+>(.*?)</a>'
unknownPat=r'\[\[(.*?)\]\]'
def clean(ins):
    ins1=re.sub(unknownPat,r'\1',re.sub(linkPat,r'\1',ins))
    return ins if ins==ins1 else clean(ins1)

def c(ws,r,c,v,style=None,nbFmt=None,font=None):
    cell=ws.cell(row=r,column=c,value=v)
    cell.alignment = Alignment(wrapText=True,vertical="top")
    if style!=None:
        cell.style=style
    if nbFmt!=None:
        cell.number_format=nbFmt
    if font!=None:
        cell.font=font

def createExcelWB(amrs,bleuScore):    
    wb = Workbook()
    ws = wb.active
    courier10=Font(name='courier',size=10)
    bold=Font(bold=True)
    c(ws,1,1,"Id",font=bold)
    c(ws,1,2,"AMR",font=bold)
    c(ws,1,3,"Base gen",font=bold)
    c(ws,1,4,"Original",font=bold)
    c(ws,1,5,"Generated",font=bold)
    c(ws,1,6,"Score",font=bold)
    c(ws,1,7,"Comment",font=bold)
    ws.column_dimensions["A"].width=10
    ws.column_dimensions["B"].width=80
    ws.column_dimensions["C"].width=30
    ws.column_dimensions["D"].width=30
    ws.column_dimensions["E"].width=30
    r=2
    for amr in amrs:
        c(ws,r,1,amr["id"])
        c(ws,r,2,amr["AMR"].rstrip(),font=courier10)
        c(ws,r,3,amr["basegen"])
        c(ws,r,4,amr["snt"])
        c(ws,r,5,clean(amr["gophi"]),font=bold)
        c(ws,r,6,"")
        r+=1
    last=r-1
    r+=1
    c(ws,r,2,"BLEU score")
    c(ws,r,4,bleuScore,nbFmt="0.00")
    c(ws,r,6,"=AVERAGE(F2:F%s)"%last,nbFmt="0.00")
    notes=["Generator error","Gibberish/Missing info","Barely understandable","OK, but bad syntax","Perfect"]
    for i in range(4,-1,-1):
        r+=1
        c(ws,r,4,notes[i])
        c(ws,r,5,i)
        c(ws,r,6,"=COUNTIF(F$2:F$%d,$E%d)"%(last,r))
        c(ws,r,7,"=F%d/F%d"%(r,last+8),"Percent","0%")
    r+=1
    c(ws,r,6,"=SUM(F%d:F%d)"%(r-5,r-1))
    r+=1
    dateTime=datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
#     print("%d,%d=%s"%(r,1,dateTime))
    c(ws,r,2,"Created: "+dateTime)
    c(ws,r,4,"Not yet evaluated")
    c(ws,r,6,'=COUNTIF(F$2:F$%d,"")'%last)
    
    ws.column_dimensions['C'].hidden= True # cacher la colonne BaseGen 
    return wb    

specialPat=re.compile(r'^# ::(gophi|snt|id|basegen) ?(.*)')

def getAMRs(fileName):
    amrs=[]
    amr={"AMR":""}
    with open(fileName,encoding="utf-8") as fp:  
        line = fp.readline()
        while len(line)>0:
            line=line.rstrip()
            if len(line)==0:
                if len(amr["AMR"])>0:
                    amrs.append(amr)
                    amr={"AMR":""}
            else: 
                m=specialPat.match(line)
                if m:
                    if m.group(1)=="id":
                        amr["id"]=m.group(2).split()[0]
                    else:
                        amr[m.group(1)]=m.group(2) if len(m.group(2))>0 else "*no text*"
                elif line[0]!="#":
                    amr["AMR"]+=line+"\n"
            line = fp.readline()
    return amrs

def showStatistics(amrs):
    candidate=[]
    reference=[]
    nbGenWords=0
    nbOriginalWords=0
    for amr in amrs:
        # print(amr)
        generated=clean(amr["gophi"])
        nbGenWords+=len(generated.split())
        candidate.append(generated)
        reference.append(amr["snt"])
        nbOriginalWords+=len(amr["snt"].split())
        # print("%d: %s"%compareLevenshtein(amr['Original'], generated))
    bleuScore=BLEU(candidate,[reference])
    nb=len(candidate)
    print("%d examples, BLEU:%5.3f"%(nb,bleuScore))
    print("mean length of original sentence:%5.2f"%(nbOriginalWords/nb))
    print("mean length of generated sentence:%5.2f"%(nbGenWords/nb))
    return bleuScore
    

if __name__ == '__main__':
    if len(sys.argv)>1:
        fileName=sys.argv[1];
    else: # for unit testing 
        fileName="/Users/lapalme/Dropbox/PourOrigene/AMR/amr-ISI/amr-examples.out"
    amrs=getAMRs(fileName)
    # print(amrs)
    bleuScore=showStatistics(amrs)
    ## générer la feuille excel pour l'évaluation
    wb=createExcelWB(amrs,bleuScore)        
    wb.save(fileName.replace(".out",".xlsx"))

