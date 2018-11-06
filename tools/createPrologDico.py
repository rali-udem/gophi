#!/usr/bin/python3
# coding=utf-8
# from collections import Counter

### Generate prolog dictionary from Propbank frames and combine it with jsRealB lexicon

import xml.etree.ElementTree as ET 
import re,os,json,sys,time

######  Global variables
amrDir="/Users/lapalme/Dropbox/PourOrigene/AMR/"
prologDir=amrDir+"AMRtoENG/"
frameDir=amrDir+"abstract_meaning_representation_amr_2.0/data/frames/propbank-frames-xml-2016-03-08/"

nounPattern='(\'D\':D)^(\'A\':A)^np($D/d("the"),A,n("{0}"))'
adjPattern='a("{0}")'
proPattern='pro("{0}")'
detPattern='d("{0}")'
conjPattern='c("{0}")'
advPattern='(\':op1\':X)^advp(adv("{0}"),X)'
prepPattern='(\':op1\':X)^pp(p("{0}"),X)'
verbPattern='(\'ARG0\':A0)^(\'ARG1\':A1)^(\'ARG2\':A2)^(A0,vp(v("{0}"),A1,A2))'
sep="\n%% ======== %s \n"

lexicon={}
nbMissing={"N":0,"A":0,"V":0,"P":0}
nbFound  ={"N":0,"A":0,"V":0,"P":0}

nouns={}
adjectives={}
verbs={}
verbsInPB=set() # verbs in propbank
prepositions=set([l.strip() 
                 for l in open(amrDir+"data/prepositions.txt").readlines() if l[0]!="#"])
usedPrepositions=set()
## list of adverbs (or pseudo adverbs) that can appear after a verb
##   for the moment all "compound" nouns and adjectives are ignored 
allAdverbs=set(["in","out","over","under","up","down","on","off","about","into","for",
                "around","away","back","through","along","to","across","by","apart",
                "upon","after","forward","behind","even","aback"])
# print(sorted(allAdverbs))

def checkInLexicon(word,cat):
    if word not in lexicon or cat not in lexicon[word]:
#         print "%s :: %s"%(cat,word)
        nbMissing[cat]+=1
        return False
    else:
        nbFound[cat]+=1
#         del lexicon[word][cat]
        return True

# remove framenumber if it exists
def removeFN(w):
    m=re.match(r"([a-z]+)-\d+$",w)
    if m:return m.group(1)
    return w

def checkAdverb(rolesetId):
    parts=rolesetId.split("_")
    if len(parts)!=2:
        print("*** strange splitting in "+rolesetId)
        return None
    if parts[1] in allAdverbs:
        return parts
    print("*** strange preposition in "+rolesetId)
    return None

def addNoun(xmlfile,rolesetId,noun):
    if "_" in noun:return
#         print("*** name with preposition:"+noun+" in "+rolesetId)
    checkInLexicon(noun, "N")
    if removeFN(rolesetId)!=noun: # use noun if rolesetId is different
        rolesetId=noun
    nouns[rolesetId]=(nounPattern.format(noun),xmlfile)

def addAdjective(xmlfile,rolesetId,adjective):
    if "_" in adjective:return
#         print("*** adjective with preposition:"+adjective+" in "+rolesetId)
    checkInLexicon(adjective, "A")
    if removeFN(rolesetId)!=adjective: # use adjective if rolesetId is different
        rolesetId=adjective
    adjectives[rolesetId]=(adjPattern.format(adjective),xmlfile)

def addVerb(xmlfile,rolesetId,verb,args):
    if re.match('.*-91$',rolesetId):
        print(rolesetId+" ignored because special in "+xmlfile)
        return
    adverb=None
    if "_" in verb:
#         print("*** verb with adverb:"+verb+" in "+rolesetId)
        r=checkAdverb(verb)
        if r!=None:
#             print("parts:%s"%r)
            verb=r[0].replace("_","-")
            rolesetId=rolesetId.replace("_","-")
            adverb=r[1]
        else:
            return
    verbsInPB.add(verb)
    checkInLexicon(verb, "V")
    pat="verb('%s',"%rolesetId
    ## générer les lambda pour les arguments
    pat+="".join(["(':ARG{0}':X{0})^".format(i) for i in range(0,6) if 'ARG%d'%i in args])
    pat+='s('
    start=1
    # le premier argument est le sujet!!
    if "ARG0" in args: 
        pat+='X0,'
    elif "ARG1" in args:
        pat+='X1,'
        start=2
    pat+='vp(v("%s")'%verb
    if adverb!=None:
        pat+=',adv("%s")'%adverb
    for i in range(start,6):
        argi="ARG"+str(i)
        if argi in args:
            # tenter de trouver la bonne préposition en examinant les prépositions dans les exemples
            # pour le moment prendre la première...
            prep=args[argi]["prep"] 
            if len(prep)==0:
                pat+=',X%d'%i
            else:  
                mostCommonPrep=prep[0]
                if mostCommonPrep=="**":
                    pat+=',X%d'%i
                else:
                    pat+=',X{0}/pp(p("{1}"),X{0})'.format(i,mostCommonPrep)              
# version précédente qui essayait de prendre la plus fréquente en utilisant des Counter               
#                 if i==1 and mostCommonPrep=="**": # on rencontre ARG1 le plus souvent sans préposition
#                     pat+=',":%s"'%argi
#                 else:
#                     if mostCommonPrep=="**" and len(prep)>1: # chercher le suivant
#                         mostCommonPrep=prep.most_common(2)[1][0]
#                     else:
#                         mostCommonPrep=""
#                     if mostCommonPrep=="":
#                         pat+=',":%s"'%argi
#                     else:
#                         pat+=',opt(":%s",pp(p("%s"),":%s"))'%(argi,mostCommonPrep,argi)
    pat+=')))'
    verbs[rolesetId]=(pat, 
         # ajouter les noms des arguments pour les commentaires
         " / ".join([(name.upper()+":"+val["descr"]) for (name,val) in list(args.items())]),
         xmlfile)

def showTable(out,table,name):
    for (id,val) in sorted(table.items()):
        out.write("%s('%s',%s). %% [%s]"%(name,id,val[0],val[1])+"\n")

def showVerbs(out):
    for (id,val) in sorted(verbs.items()):
        out.write("%%  %s [%s]\n%s.\n\n"%(val[1],val[2],val[0]))
    
def parseXML(filename):
    root=ET.parse(open(filename,encoding='utf-8')).getroot()
    xmlfile=re.sub(".*/(.*)$","\\1",filename)
    for predicate in root.iter("predicate"):
#         print("%s:%s"%(filename,predicate.attrib["lemma"]))
        for roleset in predicate.iter("roleset"):
            rolesetId=roleset.attrib["id"].replace(".","-")
#             print(rolesetId)
            for alias in roleset.iter("alias"):
                pos=alias.attrib["pos"].lower()
                if pos=="n":
                    if rolesetId not in nouns:# n'ajouter que le premier rôle qui est le plus courant
                        addNoun(xmlfile,rolesetId,alias.text)
                elif pos=="j":
                    if rolesetId not in adjectives:
                        addAdjective(xmlfile,rolesetId,alias.text)
                elif pos=="v":
                    args={}
                    for role in roleset.iter("role"):
                        args["ARG"+role.attrib["n"]]={"descr":role.attrib["descr"],"prep":[]}
                    # passer dans les exemples pour récupérer la préposition associée à chaque argument
                    for example in roleset.iter("example"):
                        for arg in example.iter("arg"):
                            argn="ARG"+arg.attrib["n"]
                            if (argn) in args:
                                text=arg.text
                                if text!=None:
                                    firstWord=text.split()[0]
#                                     print "arg:%s:%s"%(text,firstWord)
                                    if firstWord in prepositions:
                                        args[argn]["prep"].append(firstWord)
                                        usedPrepositions.add(firstWord)
                                    else: ## compter le nombre de fois SANS préposition
                                        args[argn]["prep"].append("**")
#                     print("%s:%s:%s"%(rolesetId,alias.text,args))                    
                    if rolesetId not in verbs: 
                        addVerb(xmlfile,rolesetId,alias.text,args)
                elif pos=="l":
                    pass # ignore locutions
                else:
                    print(filename+":strange POS:"+pos)

def parseFrameFiles(frameDir):
    i=0
    for file in os.listdir(frameDir):
    # for file in ["abandon.xml"]:
        if re.fullmatch(r"[-a-z]+.xml",file): 
            i+=1
            # if (i%1000==0):print(file) # show progress of file reading
            parseXML(frameDir+"/"+file)
    #         if i==10:break
        else:
            print("Ignored file:"+file)
 

def generateDict(out):
    out.write(":-encoding(utf8).\n")
    out.write("%  "+"created by "+__file__+"\n");
    out.write("%  "+time.strftime("%c") +"\n")
    out.write("% Frames from: "+frameDir+"\n")
    out.write("%\n")
    out.write("%% Words in the propBank dictionaries compared with the jsReal dictionary in\n")
    out.write("%% "+jsrLexiconFile+"\n")
    out.write("%\n")
    out.write("%% %1s:%5s:%5s:%5s\n"%("","ABS","OK","TOTAL"))
    for cat in list(nbMissing.keys()):
        out.write("%% %s:%5d:%5d:%5d\n"%
                 (cat,nbMissing[cat],nbFound[cat],nbMissing[cat]+nbFound[cat]))
    out.write("\n")
    for pos in ["noun/2","adjective/2","verb/2","adverb/2","preposition/2","conjunction/2",
                "pronoun/2","determiner/2",
                "verbalization/2","verbalization/4"]:
        out.write(":- discontiguous %s.\n"%pos)
 
    out.write(sep%"NOUNS")
    showTable(out,nouns,'noun')
    out.write(sep%"ADJECTIVES")
    showTable(out,adjectives,'adjective')
    out.write(sep%"VERBS")
    showVerbs(out)

    out.write(sep%"New Words from the jsReal lexicon")
    for key in sorted(lexicon.keys()):
        if re.search(r'[^-a-z]',key,re.I):
            print("%s: not all letters"%key)
        else:
            cats=lexicon[key]
            for cat in cats:
                if cat=="N":
                    out.write(("noun('{0}',%s).\n"%nounPattern).format(key))
                elif cat=="V":
                    if key in verbsInPB:
                        # print("%s already in prop bank"%key)
                        pass
                    elif "N" in cats:
                        print ("Verb "+key+" ignored because it is also a noun")
                    elif "A" in cats:
                        print ("Verb "+key+" ignored because it is also an adjective")
                    else:
                        out.write(("verb('{0}',%s).\n"%verbPattern).format(key))
                elif cat=="A":
                    out.write(("adjective('{0}',%s).\n"%adjPattern).format(key))
                elif cat=="C":
                    out.write(("conjunction('{0}',%s).\n"%conjPattern).format(key))
                elif cat=="Adv":
                    out.write(("adverb('{0}',%s).\n"%advPattern).format(key))
                elif cat=="P":
                    out.write(("preposition('{0}',%s).\n"%prepPattern).format(key))
                elif cat=="Pro":
                    out.write(("pronoun('{0}',%s).\n"%proPattern).format(key))
                elif cat=="D":
                    out.write(("determiner('{0}',%s).\n"%detPattern).format(key))
                
                else:
                    print("%s:%s ignored"%(cat,key))
                # pass
# sys.exit(1)

def addMorphVerbalizations(out):
    morphVerbalizationFile=amrDir+"data/morph-verbalization-V1.01.txt"
    morphVerbRE=r'::DERIV-VERB "(.*?)" ::DERIV-NOUN(-ACTOR)? "(.*?)"( ::DERIV-NOUN(-ACTOR)? "(.*?)")?$' 
    out.write(sep%'MORPHVERBALIZATIONS')
    out.write("% morphVerb(Key,NounVerb,ActorVerb)\n")
    # out.write("morphVerbalizations={\n")
    for line in open(morphVerbalizationFile,encoding="utf-8"):
        line=line.strip()
        if line.startswith("#"):continue
        m=re.match(morphVerbRE, line)
        if m:
            infos={}
            infos["noun" if m.group(2)==None else "actor"] = m.group(3)
            if m.group(4)!=None:
                infos["noun" if m.group(5)==None else "actor"] = m.group(6)
    #         out.write(" '%s':%s,\n"%(m.group(1),infos))
            key=m.group(1)
            if "noun" in infos and infos["noun"]==key:continue # éviter des verbalisations identiques...
            if "actor" in infos and infos["actor"]==key: continue 
            out.write("morphVerb('%s',%s,%s).\n"%(m.group(1),
                                              repr(infos["noun"]) if "noun" in infos else 'null',
                                              repr(infos["actor"]) if "actor" in infos else 'null'))
        else:
            print(line+":no match in morphVerbalization")

verbalizations={}
def addVbn(concept,role,value,vrb):
    try:
        if concept in verbalizations:
            if role in verbalizations[concept]:
                verbalizations[concept][role][value]=vrb
            else:
                verbalizations[concept][role]={value:vrb}
        else:
            verbalizations[concept]={role:{value:vrb}}
    except TypeError as err:
        print(err)
        print("addVbn(%s,%s,%s,%s)"%(concept,role,value,vrb))
        for (key,value) in sorted(verbalizations.items()):
            print(key+":"+str(value))

        
def addVerbalizations(out):
    ## process only "good" verbalization
    ##  depending on the length of the second "list"
    ##  methylate-01:{"":"methylation"}                             # 1:simple case
    ##  person:{":ARG0-of":{"abuse-01":"abuser",                    # 3: single constraint
    ##                      "keep-01":[":ARG1","bee","beekeeper"] } # 5: complex constraint (not yet implemented)
    ##    
    verbalizationFile=amrDir+"data/verbalization-list-v1.06.txt"
    verbRE=r'^VERBALIZE (.*?) TO (.*)'
    ignoreRE=r'^(#|DO-NOT-|MAYBE)'
    out.write(sep%'VERBALIZATIONS')
    # out.write("verbalizations=\\\n")
    for line in open(verbalizationFile,encoding="utf-8"):
        line=line.strip()
    #     print(line)
        if re.match(ignoreRE,line):continue
        m=re.match(verbRE,line)
        if m:
            conceptArgs=m.group(2).split()
            concept=conceptArgs[0]
            if len(conceptArgs)==1:
                verbalizations[concept]={"":m.group(1)}
            elif len(conceptArgs)==3:
                addVbn(concept,conceptArgs[1],conceptArgs[2],m.group(1))
            else: ## when len==5 not yet implemented...
                pass
    #             print("not yet implemented")
        else:
            print(line+":no match")
    # pprint.pprint(verbalizations,stream=sys.stdout)
    for (key,values) in sorted(verbalizations.items()):
    #     print("key=%s; values=%s"%(key,values))
        for (k,v) in values.items():
            if k=='':
                out.write("verbalization('%s','%s').\n"%(key,v))
            else:
                k2=re.sub(r":ARG(\d)-of",r":*:ARG\1", k) # change key of inverse arg
                for (k1,v1) in v.items():
                    out.write("verbalization('%s','%s','%s','%s').\n"%(key,k2,k1,v1))


## name of program [jsrLexiconName] [generatedDictionary]
if __name__ == '__main__':
    # print("arguments:"+str(sys.argv))
    jsrLexiconFile=amrDir+"jsRealB2/lexicon-dme.json"
    if len(sys.argv)>1:jsrLexiconFile=sys.argv[1]
    outFile=prologDir+"dictionaryGenerated.pl"
    if len(sys.argv)>2:outFile=sys.argv[2]
    
    lexicon=json.load(open(jsrLexiconFile))
    parseFrameFiles(frameDir)
    for prep in usedPrepositions:
        checkInLexicon(prep, "P")
    print("\nCreating "+outFile)
    out=open(outFile,"w",encoding='utf-8')
    generateDict(out)
    addMorphVerbalizations(out)
    addVerbalizations(out)