#!/usr/bin/python3
# coding=utf-8
# from collections import Counter

import xml.etree.ElementTree as ET 
import re,os,json,sys,time

######  Global variables
amrDir="/Users/lapalme/Dropbox/PourOrigene/AMR/"
prologDir=amrDir+"AMRtoENG/"
frameDir=amrDir+"abstract_meaning_representation_amr_2.0/data/frames/propbank-frames-xml-2016-03-08/"

lexicon={}
nbMissing={"N":0,"A":0,"V":0,"P":0}
nbFound  ={"N":0,"A":0,"V":0,"P":0}

frameNumberRE=r"([-a-z]+?)-(\d|y)+$"

nouns={}
adjectives={}
verbs={}
prepositions=set([l.strip()
                 for l in open(amrDir+"data/prepositions.txt").readlines() if l[0]!="#"])
usedPrepositions=set()
## list of adverbs (or pseudo adverbs) that can appear after a verb
##   for the moment all "compound" nouns and adjectives are ignored
allAdverbs=set(["in","out","over","under","up","down","on","off","about","into","for",
                "around","away","back","through","along","to","across","by","apart",
                "upon","after","forward","behind","even","aback"])
# print(sorted(allAdverbs))
specialConcepts=['amr-choice', 'amr-unintelligible', 'amr-unknown', 'amr-empty', 'correlate-91', 'date-entity', 'date-interval', 'distance-quantity', 'email-address-entity', 'even-as', 'even-if', 'even-when', 'fluid-ounce', 'have-degree-91', 'have-mod-91', 'have-org-role-91', 'have-polarity-91', 'have-purpose-91', 'have-quant-91', 'hyperlink-91', 'include-91', 'instead-of-91', 'monetary-quantity', 'multi-sentence', 'more-than', 'natural-object', 'next-to', 'ordinal-entity', 'percentage-entity', 'phone-number-entity', 'rate-entity-91', 'ratio-of', 'relative-position', 'request-confirmation-91', 'score-entity', 'score-on-scale-91', 'seismic-quantity', 'several', 'street-address-91', 'string-entity', 'temporal-quantity', 'this', 'truth-value', 'url-entity', 'value-interval', 'volume-quantity'] + \
['sunday','monday','tuesday','wednesday','thursday','friday','saturday'] + \
["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"] 

def checkInLexicon(word,cat):
    if word not in lexicon:
        print ("%s :: %s:%d"%(cat,word))
        nbMissing[cat]+=1
        return False
    else:
        nbFound[cat]+=1
#         del lexicon[word][cat]
        return True

# remove framenumber if it exists
def removeFN(w):
    m=re.match(frameNumberRE,w)
    if m:return m.group(1)
    return w


def parseXML(filename,roleSetIds):
    root=ET.parse(open(filename,encoding='utf-8')).getroot()
    xmlfile=re.sub(".*/(.*)$","\\1",filename)
    for predicate in root.iter("predicate"):
#         print("%s:%s"%(filename,predicate.attrib["lemma"]))
        for roleset in predicate.iter("roleset"):
            roleSetId=roleset.attrib["id"].replace(".","-")
            if roleSetId in roleSetIds:
                print("duplicate %s found in %s previously found in %s"%(roleSetId,xmlfile,roleSetIds[roleSetId][0]))
            else:
                pos=set()
                for alias in roleset.iter("alias"):
                    pos.add(alias.attrib["pos"])
                roleSetIds[roleSetId]=(xmlfile,pos)

def parseFrameFiles(frameDir):
    roleSetIds={}
    i=0
    for file in os.listdir(frameDir):
    # for file in ["abandon.xml"]:
        if re.fullmatch(r"[-a-z]+.xml",file):
            i+=1
            if (i%1000==0):print(file) # show progress of file reading
            parseXML(frameDir+file,roleSetIds)
    #         if i==10:break
        else:
            print("Ignored file:"+file)
    return roleSetIds;

def compareConceptsFrames(conceptsFile,roleSetIds,jsrLexiconFile):
    lexiconS=open(jsrLexiconFile).read()
    lexicon=json.loads(lexiconS[lexiconS.index('\n'):]) # skip first line which is not json
    
    concepts={}
    for cc in open(conceptsFile).read().split("\n"):
        if len(cc)<=1:continue
        [concept,count]=cc.split("\t")
        concepts[concept]=int(count)
    
        
    # print("Concepts with frame number not found in frames")
    # for conceptKey in sorted(concepts.keys()):
    #     if re.fullmatch(frameNumberRE,conceptKey) and conceptKey not in roleSetIds:
    #         print(conceptKey+":"+str(concepts[conceptKey]))
    # print("---")
    
    nbConceptsFound=0
    nbConceptsNotFound=0
    nbSpecialConceptsFound=0
    conceptsFound=[]
    conceptsNotFound=[]
    print("Concepts without frame numbers not found in lexicon")
    for conceptKey in sorted(concepts.keys()):
        if conceptKey in specialConcepts:
            nbSpecialConceptsFound+=concepts[conceptKey]
        elif not re.fullmatch(frameNumberRE,conceptKey):
            # print("conceptKey:"+conceptKey)
            if "-" in conceptKey:
                words=conceptKey.split("-")
                found=True
                for i in range(0,len(words)):
                    w=words[i]
                    if (w not in lexicon) and (w not in prepositions) and (w not in allAdverbs):
                        found=False
                        break
            else:
                found=conceptKey in lexicon
            if found:
                nbConceptsFound+=concepts[conceptKey]
                conceptsFound.append(conceptKey)
            else:
                nbConceptsNotFound+=concepts[conceptKey]
                conceptsNotFound.append(conceptKey)
    
    print("Concepts spéciaux: %d occurrences"%(nbSpecialConceptsFound))
    print("Concepts trouvés:%d pour %d occurrences"%(len(conceptsFound),nbConceptsFound))
    print("Concepts non trouvés: %d pour %d occurrences"%(len(conceptsNotFound),nbConceptsNotFound))
    # print("Concepts Found")
    # print(conceptsFound)
    # print("Concepts NOT Found")
    # print(conceptsNotFound)
        

## name of program [jsrLexiconName] [generatedDictionary]
if __name__ == '__main__':
    roleSetIds=parseFrameFiles(frameDir)
    jsrLexiconFile=amrDir+"jsRealB2/lexicon-en.js"
    compareConceptsFrames(amrDir+"amr-ISI/conceptCounts.txt",roleSetIds,jsrLexiconFile)
    print("====")
    jsrLexiconFile=amrDir+"jsRealB2/lexicon-dme.js"
    compareConceptsFrames(amrDir+"amr-ISI/conceptCounts.txt",roleSetIds,jsrLexiconFile)
    
    
