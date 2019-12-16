:-encoding(utf8).
%%% process a single role by augmenting the environment and the list of options

:- discontiguous processRole/8.

isArgOp(Role):-re_match('^:(ARG|op)\\d$',Role).% beware of not catching inverse role
%% frame (i.e. :ARGi) or :opI roles
processRole(Role,_OuterConcept,OuterPOS,AMR,Env,OptionsIn,[Role:DSyntRout|Env],OptionsOut):-
    isArgOp(Role),!, 
    amr2dsr(AMR,_Concept,POS,DSyntRin),
    processFrameOpRole(OuterPOS,POS,Role,DSyntRin,OptionsIn,DSyntRout,OptionsOut).
 processFrameOpRole('Verb',_POS,_Role,DSyntR,Options,pp(p("to"),DSyntR1),Options):-
     DSyntR=..[s,null,VP],               % infinitive when the sentence is a verb without subject
     VP=..[vp,v(V)|Comp],DSyntR1=..[vp,v(V)*t("b")|Comp]. 
 processFrameOpRole('Verb',_,Role,"*unknown*",Options,null,[typ({"int":Type})|Options]):-
     questionType(Role,Type).
 processFrameOpRole('Verb',_,Role,s("*unknown*",DSyntR),Options,DSyntR,[typ({"int":Type})|Options]):-
     questionType(Role,Type).
 processFrameOpRole('Verb',_POS,_Role,DSyntR,Options,sp(pro("that"),DSyntR),Options):-
     isS(DSyntR). % insert "that" when the argument is a sentence
 processFrameOpRole(_,'Pronoun',Role,pro("I")*pe(PE)*g(G)*n(N),Options,
                                     pro("me")*pe(PE)*g(G)*n(N),Options):-
        \+memberchk(Role,[':ARG0',':ARG1']). % change pronoun I for me for ARGi i>1
        % but sometimes :ARG1 should be changed if human "patient" if :ARG0 exists, 
        % but this info is not available here (some simple cases are dealt with checkAccusativePronoun/2 )
 processFrameOpRole(_,_,_,DSyntR,Options,DSyntR,Options).

% HACK: rough classification of question types based on the "usual" arguments...
questionType(':ARG0',"wos").
questionType(':ARG1',"wad").
questionType(':ARG2',"how").
questionType(':ARG3',"whe").
questionType(':ARG4',"woi").
questionType(Role,"wos"):-writeln('** unimplemented role for amr-unknown':Role).

%% transformed inverse role
processRole(StarRole,OuterConcept,_OuterPOS,AMR,Env,Options,EnvOut,OptionsOut):-
     atom_concat(':*',Role,StarRole),!,
     ((hasShortCutRole(AMR,Role,Role1,SCRole), 
       AMR = [_,_|Roles0],hasRole(Roles0,Role1,AMR0,_Roles1),
       processRole(SCRole,OuterConcept,_,AMR0,Env,Options,EnvOut,OptionsOut));
      (findRelPronoun(Role,OuterConcept,Pronoun),
       amr2dsr(AMR,_Concept,POS,DSyntR1),
       processStarRole(POS,Pronoun,DSyntR1,OutDSyntR0),
       EnvOut=[':*':OutDSyntR0|Env],OptionsOut=Options)).
 processStarRole('Noun',Pronoun,DSyntR1,DSyntR2):-
     isS(DSyntR1)->addFirstChildS(DSyntR1,Pronoun,DSyntR2);
          predicate(Pronoun,DSyntR1,DSyntR2).
 processStarRole('Adjective',Pronoun,DSyntR1,DSyntR2):-
     isS(DSyntR1)->addFirstChildS(DSyntR1,Pronoun,DSyntR2);
          predicate(Pronoun,DSyntR1,DSyntR2).
 processStarRole(_,Pronoun,DSyntR1,ls(Pronoun,DSyntR1)).

 findRelPronoun(':ARG0',Concept,pro("who")) :- gender(Concept,G),G\="n".
 findRelPronoun(':ARG1',Concept,pro("who")) :- gender(Concept,G),G\="n".
 findRelPronoun(_Role,Concept,  pro("whom")):- gender(Concept,G),G\="n".
 findRelPronoun(_Role,_Concept, pro("that")).

 addFirstChildS(In*Option,Child,Out*Option):-addFirstChildS(In,Child,Out).
 addFirstChildS(In,Child,Out):-In=..[s|Cs],Out=..[s,Child|Cs].

%% special inverse role generated by editor shortcuts
%%   https://www.isi.edu/~ulf/amr/lib/amr-dict.html#shortcuts
%% currently only cause-01 is implemented...
 hasShortCutRole(['cause-01'|_],':ARG1',':ARG0',':cause').

% %% other special roles
processRole(':accompanier',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("with",DSyntR,PPDSyntR).

processRole(':age',_OuterConcept,_OuterPOS,['amr-unknown',_],
            Env,Options,[':&':q("how old")|Env],[a("?")|Options]).
processRole(':age',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(DSyntR,q("old"))|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':beneficiary',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("for",DSyntR,PPDSyntR).

processRole(':cause',_OuterConcept,_OuterPOS,['amr-unknown',_],
            Env,Options,Env,[typ({"int":"why"})|Options]).
processRole(':cause',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':Conj|Env],Options):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    (memberchk(POS,['Special','Noun'])->Conj=sp(c("because"),p("of"),DSyntR);
               Conj=sp(c("because"),DSyntR)).

processRole(':compared-to',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(q("compared to"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':concession',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    processConcession(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processConcession(DSyntR,Env,Options,[':*':DSyntR|Env],Options):-
     (DSyntR=..[ls|_];DSyntR=..[q,"after all"|_]),!.
 processConcession(DSyntR,Env,Options,[':*':ls(q("despite"),DSyntR)|Env],Options).


processRole(':condition',_OuterConcept,_OuterPOS,['as-long-as',_,[':op1',OP1]],
            Env,Options,[':*':ls(q("as long as"),DSyntR)|Env],Options):-
    amr2dsr(OP1,_Concept,_POS,DSyntR).
processRole(':condition',_OuterConcept,_OuterPOS,['otherwise',_,Cond],
            Env,Options,[':*':sp(c("otherwise"),DSyntR)|Env],Options):-
    amr2dsr(Cond,_Concept,_POS,DSyntR).
processRole(':condition',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':sp(c("if"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':consist-of',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("of",DSyntR,PPDSyntR).

processRole(':degree',_OuterConcept,_OuterPOS,[DEG,_],
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    % do not evaluate AMR...
    processDegree(DEG,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processDegree('amr-unknown',Env,Options,Env,[typ({int:"how"})|Options]):-!.
 processDegree('more',Env,Options,Env,[f("co")|Options]):-!.
 processDegree('most',Env,Options,Env,[f("su")|Options]):-!.
 processDegree('part',Env,Options,[':&':adv("partially")|Env],Options):-!.
 processDegree(DEG,Env,Options,[':&':adv(DEGs)|Env],Options):-
     memberchk(DEG,['too','so','very','all','quite']),!,atom_string(DEG,DEGs).
 processDegree(DEG,Env,Options,[':*':adv(DEGs)|Env],Options):-
     adverb(DEG,_Adv),!,atom_string(DEG,DEGs).
 processDegree(DEG,Env,Options,[':*':Adv|Env],Options):-
     adjective(DEG,_),!,adj2adv(a(DEG),Adv).
 processDegree(DEG,Env,Options,[':*':q(DEGs)|Env],Options):-
     cleanConcept(DEG,DEGs).

processRole(':destination',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("to",DSyntR,PPDSyntR).

processRole(':direction',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("to",DSyntR,PPDSyntR).

processRole(':domain',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processSimpleDomain(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    (amr2dsr(AMR,_Concept,_POS,DSyntR),
     EnvOut=[':*':vp(v("be"),DSyntR)|EnvIn],OptionsOut=OptionsIn).
 processSimpleDomain([Det,_],Env,Options,['D':DetS|Env],Options):-
     determiner(Det,DetS).
 processSimpleDomain([Pro,_],Env,Options,[':&':ls(ProDSyntR,v("be"))|Env],Options):-
     pronoun(Pro,ProDSyntR).

processRole(':domain-of',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processRole(':mod',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut).

processRole(':duration',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    processDuration(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processDuration("*unknown*",Env,Options,[':&':q("How long")|Env],[a("?")|Options]).
 processDuration(DSyntR,Env,Options,[':*':pp(p("for"),DSyntR)|Env],Options).

processRole(':example',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls("for example",DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':frequency',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':Freq|Env],Options):-
    AMR='1'->Freq="once";
    AMR='2'->Freq="twice";
    atom(AMR),atom_number(AMR,_)->atom_string(AMR,N),Freq=ls(N,"times");
    amr2dsr(AMR,_Concept,_POS,DSyntR),Freq=DSyntR.

processRole(':instrument',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("with",DSyntR,PPDSyntR).

processRole(':li',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':&':Val|Env],Options):-
    AMR='-1'->Val="lastly";
    AMR='1' ->Val="first";
    amr2dsr(AMR,_Concept,_POS,DSyntR),Val=ls(DSyntR,":").

processRole(':location',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    processLocation(DSyntR,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processLocation("*unknown*",_POS,Env,Options,Env,[typ({"int":"whe"})|Options]).
 processLocation(DSyntR,'Preposition',Env,Options,[':*':DSyntR|Env],Options):-!.
 processLocation(DSyntR,_POS,Env,Options,[':*':PPDSyntR|Env],Options):-
     addPrep("in",DSyntR,PPDSyntR).

processRole(':manner',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    processManner(DSyntR,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processManner("*unknown*",_,Env,Options,Env,[typ({"int":"how"})|Options]).
 processManner(q(Manner),_,Env,Options,[':*':q(Manner)|Env],Options).
 processManner(DSyntR,'Adjective',Env,Options,[':*':Adv|Env],Options):-
     adj2adv(DSyntR,Adv).
 processManner(DSyntR,'Verb',Env,Options,[':*':pp(p("by"),DSyntR*typ({"prog":true}))|Env],Options).
 processManner(DSyntR,_,Env,Options,[':*':pp(p("with"),DSyntR)|Env],Options).

processRole(':medium',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("in",DSyntR,PPDSyntR).


processRole(':mod',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    memberchk(OuterPOS,['Noun','Special']),
    processSimpleModNoun(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    processSimpleModOther(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    (checkNegMod(AMR,AMR1,IsNeg),
     amr2dsr(AMR1,_Concept,POS,DSyntR),
     (memberchk(POS,['Noun','Special'])->
         EnvOut=[':*':pp(p("of"),DSyntR)|EnvIn],OptionsOut=OptionsIn;
         relative(OuterConcept,DSyntR,Relative),
         (IsNeg=true->Relative1=Relative*typ({neg:true});Relative1=Relative),
         EnvOut=[':*':Relative1|EnvIn],OptionsOut=OptionsIn)
    ).
 %% pour les cas "simples" on modifie l'environnement du OuterConcept
 processSimpleModNoun(['amr-unknown',_],Env,Options,['D':"what"|Env],[a("?")|Options]).
 processSimpleModNoun(['all',_],Env,Options,[':&':q("all")|Env],[n("p")|Options]).
 processSimpleModNoun(['many',_],Env,Options,[':&':q("many")|Env],[n("p")|Options]).
 processSimpleModNoun(['both',_],Env,Options,[':&':q("both")|Env],[n("p")|Options]).
 processSimpleModNoun(['no',_],Env,Options,[':&':q("no")|Env],Options).
 processSimpleModNoun(['too',_],Env,Options,[':&':q("too")|Env],Options).
 processSimpleModNoun(['any',_],Env,Options,[':&':q("any")|Env],Options).
 processSimpleModNoun(['other',_],Env,Options,[':&':q("other")|Env],Options).
 processSimpleModNoun(['some',_],Env,Options,[':&':q("some")|Env],Options).
 processSimpleModNoun(['one',_],Env,Options,[':&':q("one")|Env],Options).
 processSimpleModNoun([Det,_],Env,Options,['D':DetS|Env],Options):-
     determiner(Det,DetS).
 processSimpleModNoun([Atom,_],_Env,Options,[':*',no(Number)],Options):-atom_number(Atom,Number).     
 %% suppose qu'on ne combine pas les :mod de noms ou d'adjectifs
 %%   préfixe les :mod pour les noms  
 processSimpleModNoun([Noun,_],EnvIn,Options,['A':AOut|EnvOut],Options):-
     noun(Noun,_),cleanConcept(Noun,NounC),
     (select('A':a(A),EnvIn,EnvOut),atomics_to_string([NounC,A],' ',NewA),AOut=a(NewA);
      atom_string(NounC,NewC),AOut=n(NewC),EnvOut=EnvIn).
 %% postfixe les :mod pour les adjectifs
 processSimpleModNoun([Adjective,_],EnvIn,Options,['A':a(NewA)|EnvOut],Options):-
     adjective(Adjective,_),cleanConcept(Adjective,AdjectiveC),
     (select('A':a(A),EnvIn,EnvOut),atomics_to_string([A,AdjectiveC],' ',NewA);
      atom_string(AdjectiveC,NewA),EnvOut=EnvIn).
 processSimpleModNoun([Verb,_],EnvIn,Options,[':*':v(VerbC)*t("pr")|EnvIn],Options):-
     verb(Verb,_),cleanConcept(Verb,VerbC).
 
 processSimpleModOther([MOD,_],Env,Options,[':&':q(MODs)|Env],Options):-
     cleanConcept(MOD,MODs).
 %% négation d'un mod s'applique à la relative qui sera générée...
 checkNegMod([Concept,V|Roles],[Concept,V|Roles1],true):-
     select([':polarity',-],Roles,Roles1).
 checkNegMod(AMR,AMR,false).
     
 
processRole(':mode',_OuterConcept,_OuterPOS,'expressive',
            Env,Options,Env,[a("!")|Options]).
processRole(':mode',_OuterConcept,_OuterPOS,'imperative',
            Env,Options,Env,[t("ip")|Options]).
processRole(':mode',_OuterConcept,_OuterPOS,'interrogative',
            Env,Options,Env,[typ({"int":"yon"})|Options]).

processRole(':name',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processRole(':named',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut).
processRole(':named',_OuterConcept,_OuterPOS,AMR,Env,Options,
            [':*':DSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':ord',_OuterConcept,_OuterPOS,['ordinal-entity',_|Roles],Env,Options,['A':DSyntR|Env],Options):-
    amr2dsr(['ordinal-entity',_|Roles],_Concept,_POS,DSyntR).
processRole(':ord',_,_,AMR,Env,Options,Env,Options):-
    writeln('** :ord without ordinal-entity ':AMR).

processRole(':part-of',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,Concept,POS,DSyntR),
    processPartOf(DSyntR,Concept,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processPartOf(pro(_)*pe(PE)*g(G)*n(N),_Concept,'Pronoun',Env,Options,
               ['D':d("my")*pe(PE)*g(G)*n(N)|Env],Options) :-!.
 processPartOf(DSyntR,_Concept,_POS,Env,Options,[':*':PPDSyntR|Env],Options):-
     addPrep("of",DSyntR,PPDSyntR).

processRole(':path',_OuterConcept,_OuterPos,[past,p,[':op1',OP1]],
            Env,Options,[':*':advp(adv("past"),DSyntR)|Env],Options):-
    amr2dsr(OP1,_Concept,_POS,DSyntR).
processRole(':path',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':Out|Env],Options):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    (POS='Adverb'->Out=DSyntR;Out=pp(p("via"),DSyntR)).

processRole(':polarity',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    %% DO NOT evaluate AMR
    processPolarity(AMR,OuterConcept,OuterPOS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processPolarity(['amr-unknown',_],_,_,Env,Options,Env,[typ({"int":"yon"})|Options]).
 processPolarity('+',_,_,Env,Options,Env,Options).
 processPolarity('-',_,'Noun',Env,Options,['D':q("no")|Env],Options).
 processPolarity('-',_Adj,'Adjective',Env,Options,[':&':q("not")|Env],Options).
 processPolarity('-',_Adv,'Adverb',Env,Options,[':&':q("not")|Env],Options).
 processPolarity('-',_,_,Env,Options,Env,[typ({"neg":true})|Options]).
 processPolarity(Value,_,_,Env,Options,Env,Options):-
     writeln('** processPolarity: unknown value':Value).

processRole(':polite',_OuterConcept,_OuterPOS,'+',
            Env,Options,[':&':q("Please")|Env],Options).
processRole(':polite',_OuterConcept,_OuterPOS,'-',
            Env,Options,[':&':q("F...")|Env],Options).
processRole(':polite',_OuterConcept,_OuterPOS,Value,
            Env,Options,Env,Options):-
    writeln('** processRole: strange polite value':Value).

processRole(':poss',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,Concept,POS,DSyntR),
    processPoss(DSyntR,Concept,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processPoss("*unknown*",_,_,Env,Options,[':*':pp(p("of"),pro("whom"))|Env],[a("?")|Options]).
 processPoss(DSyntR,_,'Determiner',Env,Options,['D':DSyntR|Env],Options).
 processPoss(_DSyntR,Concept,'Pronoun',Env,Options,['D':Poss|Env],Options):-
     possessive(Concept,Poss).
 processPoss(DSyntR,_Concept,_,Env,Options,[':*':PPDSyntR|Env],Options):-
     addPrep("of",DSyntR,PPDSyntR).
 % processPoss(_,_,_,Env,Options,Env,Options).

processRole(':purpose',_OuterConcept,_OuterPOS,[Concept,V,[':ARG0',_V]|Roles],
            Env,Options,[':*':pp(p("for"),DSyntR*t("pr"))|Env],Options):- 
    %% remove useless subject reference to a variable
    amr2dsr([Concept,V|Roles],_Concept,_POS,DSyntR).
processRole(':purpose',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("for"),DSyntR*t("pr"))|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':quant',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processSimpleQuant(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    processFuzzyQuant1(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    processFuzzyQuant2(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    (amr2dsr(AMR,Concept,_POS,DSyntR),
     processQuant(DSyntR,Concept,EnvIn,OptionsIn,EnvOut,OptionsOut)).
 processSimpleQuant(['most',_],Env,Options,[':&':q("most of")|Env],[n("p")|Options]).
 processSimpleQuant(['more',_],Env,Options,[':&':q("more")|Env],[n("p")|Options]).
 processSimpleQuant(['numerous',_],Env,Options,[':&':q("numerous")|Env],[n("p")|Options]).
 processSimpleQuant(['many',_],Env,Options,[':&':q("many")|Env],[n("p")|Options]).
 processSimpleQuant(['multiple',_],Env,Options,[':&':q("multiple")|Env],[n("p")|Options]).
 processSimpleQuant(['few',_],Env,Options,[':&':q("few")|Env],[n("p")|Options]).
 processSimpleQuant(['some',_],Env,Options,[':&':q("some")|Env],Options).
 processSimpleQuant(['such',_],Env,Options,[':&':q("such")|Env],Options).
 processSimpleQuant(['last',_],Env,Options,[':&':q("last")|Env],Options).
 processSimpleQuant(['one',_],Env,Options,[':&':q("one")|Env],Options).
 processSimpleQuant(['entire',_],Env,Options,[':&':q("entire")|Env],Options).
 processSimpleQuant(['lot',_],Env,Options,[':*':q("a lot")|Env],Options).
 processSimpleQuant(['sack',_],Env,Options,[':&':q("a sack of")|Env],Options).
 %% fuzzy operators from https://www.isi.edu/~ulf/amr/lib/popup/quantities.html#non-exact-quantities
 %% role :op1
 % about, above, almost, approximately, around, at-least, at-most, below, close-to, couple, few, less-than, lot, many, more-than, multiple, nearly, no-more-than, over, roughly, several, some, under, up-to
 % number is dealt separately
 fuzzyQuant1(FQ,DSyntR,ls(FQS,DSyntR)):-
     memberchk(FQ,['about', 'above', 'almost', 'approximately', 'around', 'at-least', 'at-most', 'below', 'close-to', 
                   'couple', 'few', 'less-than', 'lot', 'many', 'more-than', 'nearly', 'no-more-than', 
                   'over', 'roughly', 'several', 'some', 'under', 'up-to']),
     re_replace('-'/g,' ',FQ,FQS).
 fuzzyQuant1('multiple',DSyntR,np(n("multiple"),DSyntR,q("of"))).
 processFuzzyQuant1([FQ,_,[':op1',Op1Amr]],Env,Options,['D':DetPat|Env],[n("p")|Options]):-
     fuzzyQuant1(FQ,DSyntR,DetPat),
     amr2dsr(Op1Amr,_Concept,_POS,DSyntR).
 %% roles :op1 and :op2
 % between
 fuzzyQuant2('between',DSyntR1,DSyntR2,ls(p("between"),DSyntR1,c("and"),DSyntR2)).
 processFuzzyQuant2([FQ,_,[':op1',Op1Amr],[':op2',Op2Amr]],Env,Options,
                    ['D':DetPat|Env],[n("p")|Options]):-
     fuzzyQuant2(FQ,DSyntR1,DSyntR2,DetPat),
     amr2dsr(Op1Amr,_Concept1,_POS1,DSyntR1),amr2dsr(Op2Amr,_Concept2,_POS2,DSyntR2).
 %% other roles
 processQuant("*unknown*",_Concept,Env,Options,[':&':q("much")|Env],[typ({"int":"how"})|Options]):-!.
 processQuant(q(NumA),_Concept,Env,Options,['D':no(Num)|Env],Options):-
        atom_number(NumA,Num),!.
 processQuant(DSyntR,'temporal-quantity',Env,Options,[':&':DSyntR|Env],Options):-!.
 processQuant(DSyntR,_Concept,Env,Options,[':&':ls(DSyntR,p("of"))|Env],Options).

processRole(':source',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("from",DSyntR,PPDSyntR).

processRole(':subevent-of',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("during",DSyntR,PPDSyntR).

processRole(':time',_OuterConcept,_OuterPOS,['date-entity',V|Roles], % cas très fréquent
            Env,Options,[':*':pp(p("on"),DSyntR)|Env],Options):-!,
            amr2dsr(['date-entity',V|Roles],_Concept,_POS,DSyntR).
processRole(':time',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    processTime(DSyntR,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processTime("*unknown*",_,Env,Options,Env,[typ({"int":"whn"})|Options]).
 processTime(a("former"),_,Env,Options,[':&':adv("formerly")|Env],Options).
 processTime(q("ex"),_,Env,Options,[':&':q("ex")|Env],Options).
 processTime(DSyntR,'Verb',Env,Options,[':*':sp(c("when"),DSyntR)|Env],Options).
 processTime(DSyntR,'Adjective',Env,Options,[':*':DSyntRAdv|Env],Options):-
     adj2adv(DSyntR,DSyntRAdv).
 processTime(DSyntR,_,Env,Options,[':*':DSyntR|Env],Options).

processRole(':topic',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':PPDSyntR|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    addPrep("about",DSyntR,PPDSyntR).

processRole(':wiki',_OuterConcept,_OuterPOS,'-', %ignore when no value
            Env,Options,Env,Options).
processRole(':wiki',_OuterConcept,_OuterPOS,AMR,
            Env,Options,Env,[tag("a",{"href":("https://en.wikipedia.org/wiki/"+Value)})|Options]):-
    amr2dsr(AMR,_Concept,_POS,Value).

processRole(Role,_OuterConcept,_OuterPOS,AMR,Env,Options,[':*':sp(c(ConjC),DSyntR)|Env],Options):-
    atom_concat(':conj-',Conj,Role),cleanConcept(Conj,ConjC),
    amr2dsr(AMR,_,_,DSyntR).

processRole(Role,_OuterConcept,_OuterPOS,AMR,Env,Options,[':*':pp(p(PrepC),DSyntR)|Env],Options):-
    atom_concat(':prep-',Prep,Role),cleanConcept(Prep,PrepC),
    amr2dsr(AMR,_,_,DSyntR).

%%%%%%% unknown role : add it to the environment
processRole(Role,_OuterConcept,_OuterPOS,AMR,Env,Options,[Role:DSyntR|Env],Options):-
    % writeln("** processRole: unknown role":Role),
    amr2dsr(AMR,_,_,DSyntR).


%%%%%%%%%%%%% add Preposition but change pronoun to accusative
addPrep(Prep,NomPronoun,pp(p(Prep),AccPronoun)):-nominative2accusativePronoun(NomPronoun,AccPronoun).
addPrep(Prep,DSyntR,pp(p(Prep),DSyntR)).

nominative2accusativePronoun(pro("I")*pe(PE)*g(G)*n(N),pro("me")*pe(PE)*g(G)*n(N)).