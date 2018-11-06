:-encoding(utf8).
%%% process a single role by augmenting the environment and the list of options

:- discontiguous processRole/8.

isArgOp(Role):-re_match('^:(ARG|op)\\d$',Role).% beware of not catching inverse role
%% frame (i.e. :ARGi) or :opI roles
processRole(Role,_OuterConcept,OuterPOS,AMR,Env,OptionsIn,[Role:DSyntRout|Env],OptionsOut):-
    isArgOp(Role),!, 
    amr2dsr(AMR,_Concept,POS,DSyntRin),
    processFrameOpRole(OuterPOS,POS,Role,DSyntRin,OptionsIn,DSyntRout,OptionsOut).
 processFrameOpRole('Verb',_POS,_Role,DSyntR,Options,DSyntR*t("b"),Options):-
     DSyntR=..[s,null|_]. % infinitive when the sentence is a verb without subject
 processFrameOpRole('Verb',_POS,_Role,DSyntR,Options,sp(pro("that"),DSyntR),Options):-
     isS(DSyntR). % insert "that" when the argument is a sentence 
 processFrameOpRole(_,'Pronoun',Role,pro("I")*pe(PE)*g(G)*n(N),Options,
                                     pro("me")*pe(PE)*g(G)*n(N),Options):-
        \+memberchk(Role,[':ARG0',':ARG1']). % change pronoun I for me for ARGi i>1
         % but sometimes :ARG1 should be changed if human "patient" if :ARG0 exists, but this info is not available here
 processFrameOpRole(_,'Noun',Role,"*unknown*",Options,null,[typ({"int":Type})|Options]):-
     questionType(Role,Type).
 processFrameOpRole(_,_,_,DSyntR,Options,DSyntR,Options).

% HACK: rough classification of question types based on the "usual" arguments...
questionType(':ARG0',"wos").
questionType(':ARG1',"wad").
questionType(':ARG2',"how").
questionType(':ARG3',"whe").
questionType(':ARG4',"woi").
questionType(Role,"wos"):-writeln('** unimplemented role for amr-unknown':Role).

%% transformed inverse role
processRole(StarRole,OuterConcept,_OuterPOS,AMR,Env,Options,[':*':OutDSyntR|Env],Options):-
    (atom_concat(':*',Role,StarRole),!,
     findRelPronoun(Role,OuterConcept,Pronoun),
     amr2dsr(AMR,_Concept,POS,DSyntR1)),
     processStarRole(POS,Pronoun,DSyntR1,OutDSyntR).
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

% %% other special roles
processRole(':accompanier',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("with"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':age',_OuterConcept,_OuterPOS,['amr-unknown',_],
            Env,Options,[':&':q("how old")|Env],[a("?")|Options]).
processRole(':age',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(DSyntR,q("old"))|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':beneficiary',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("for"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':cause',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(q("because"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':compared-to',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(q("compared to"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':condition',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':ls(Word,DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),!,
    (DSyntR=q("otherwise")->Word=null;Word=q("if")).

processRole(':concession',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    processConcession(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processConcession(DSyntR,Env,Options,[':*':DSyntR|Env],Options):-
     (DSyntR=..[ls|_];DSyntR=..[q,"after all"|_]),!.
 processConcession(DSyntR,Env,Options,[':*':ls(q("despite"),DSyntR)|Env],Options).

processRole(':consist-of',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("of"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':degree',_OuterConcept,OuterPOS,[DEG,_],
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    % do not evaluate AMR...
    processDegree(DEG,OuterPOS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processDegree('amr-unknown',_,Env,Options,[':&':adv("how")|Env],Options):-!.
 processDegree('more',_,Env,Options,Env,[f("co")|Options]):-!.
 processDegree('most',_,Env,Options,Env,[f("su")|Options]):-!.
 processDegree(DEG,_,Env,Options,[':&':adv(DEGs)|Env],Options):-
     memberchk(DEG,['too','so','very','all']),!,atom_string(DEG,DEGs).
 processDegree(DEG,'Adjective',Env,Options,[':&':adv(DEGs)|Env],Options):-
     adverb(DEG,_Adv),!,atom_string(DEG,DEGs).
 processDegree(DEG,'Adjective',Env,Options,[':&':adv(Advs)|Env],Options):-
     adjective(DEG,_),!,adj2adv(DEG,Adv),atom_string(Adv,Advs).
 processDegree(DEG,_,Env,Options,[':&':q(DEGs)|Env],Options):-
     cleanConcept(DEG,DEGs).

processRole(':destination',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("to"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':direction',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("to"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

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
            Env,Options,[':*':pp(p("with"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':li',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':&':Val|Env],Options):-
    AMR='-1'->Val="lastly";
    AMR='1' ->Val="first";
    amr2dsr(AMR,_Concept,_POS,DSyntR),Val=ls(DSyntR,":").

processRole(':location',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    processLocation(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processLocation("*unknown*",Env,Options,Env,[typ({"int":"whe"})|Options]).
 processLocation(DSyntR,Env,Options,[':*':pp(p("in"),DSyntR)|Env],Options).

processRole(':manner',_OuterConcept,_OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,POS,DSyntR),
    processManner(DSyntR,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processManner("*unknown*",_,Env,Options,Env,[typ({"int":"how"})|Options]).
 processManner(q(Manner),_,Env,Options,[':*':q(Manner)|Env],Options).
 processManner(DSyntR,'Adjective',Env,Options,[':*':Adv|Env],Options):-
     adj2adv(DSyntR,Adv).
 processManner(DSyntR,'Verb',Env,Options,[':*':ls("by",DSyntR*t("pr"))|Env],Options).
 processManner(DSyntR,_,Env,Options,[':*':ls("by",DSyntR)|Env],Options).

processRole(':medium',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("via"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).


processRole(':mod',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    OuterPOS='Noun',processSimpleModNoun(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    processSimpleModOther(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    (amr2dsr(AMR,_Concept,_POS,DSyntR),
     relative(OuterConcept,DSyntR,Relative),
         EnvOut=[':*':Relative|EnvIn],OptionsOut=OptionsIn).
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
 processSimpleModNoun([Noun,_],EnvIn,Options,['A':NewA|EnvOut],Options):-
     noun(Noun,_),cleanConcept(Noun,NounC),
     (select('A':A,EnvIn,EnvOut),atomics_to_string([NounC,A],' ',NewA);
      atom_string(NounC,NewA),EnvOut=EnvIn).
 %% postfixe les :mod pour les adjectifs
 processSimpleModNoun([Adjective,_],EnvIn,Options,['A':NewA|EnvOut],Options):-
     adjective(Adjective,_),cleanConcept(Adjective,AdjectiveC),
     (select('A':A,EnvIn,EnvOut),atomics_to_string([A,AdjectiveC],' ',NewA);
      atom_string(AdjectiveC,NewA),EnvOut=EnvIn).
 processSimpleModNoun([Verb,_],EnvIn,Options,[':*':v(VerbC)*t("pr")|EnvIn],Options):-
     verb(Verb,_),cleanConcept(Verb,VerbC).
 
 processSimpleModOther([MOD,_],Env,Options,[':&':q(MODs)|Env],Options):-
     cleanConcept(MOD,MODs).
 
processRole(':mode',_OuterConcept,_OuterPOS,'expressive',
            Env,Options,Env,[a("!")|Options]).
processRole(':mode',_OuterConcept,_OuterPOS,'imperative',
            Env,Options,[':&':q("let")|Env],[t("b")|Options]).
processRole(':mode',_OuterConcept,_OuterPOS,'interrogative',
            Env,Options,Env,[typ({"int":"yon"})|Options]).

processRole(':name',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processRole(':named',OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut,OptionsOut).
processRole(':named',_OuterConcept,_OuterPOS,AMR,Env,Options,
            [':*':DSyntR*en("\"")|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':ord',_OuterConcept,_OuterPOS,['ordinal-entity',_|Roles],Env,Options,[':*':DSyntR|Env],Options):-
    amr2dsr(['ordinal-entity',_|Roles],_Concept,_POS,DSyntR).
processRole(':ord',_,_,AMR,Env,Options,Env,Options):-
    writeln('** :ord without ordinal-entity ':AMR).

processRole(':part-of',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,Concept,POS,DSyntR),
    processPartOf(DSyntR,Concept,POS,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processPartOf(_DSyntR,Concept,'Pronoun',Env,Options,['D':Poss|Env],Options):-
     possessive(Concept,Poss).
 processPartOf(DSyntR,_Concept,_POS,Env,Options,[':*':pp(p("of"),DSyntR)|Env],Options).

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
 processPoss(DSyntR,_Concept,_,Env,Options,[':*':pp(p("of"),DSyntR)|Env],Options).
 % processPoss(_,_,_,Env,Options,Env,Options).

processRole(':purpose',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("for"),DSyntR*t("pr"))|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':quant',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processSimpleQuant(AMR,EnvIn,OptionsIn,EnvOut,OptionsOut);
    (amr2dsr(AMR,_Concept,_POS,DSyntR),
     processQuant(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut)).
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
 processQuant("*unknown*",Env,Options,[':&':q("much")|Env],[typ({"int":"how"})|Options]):-!.
 processQuant(q(NumA),Env,Options,['D':no(Num)|Env],Options):-
        atom_number(NumA,Num),!.
 processQuant(DSyntR,Env,Options,[':&':DSyntR|Env],Options).

processRole(':source',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("from"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':subevent-of',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("of"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':time',_OuterConcept,_OuterPOS,['date-entity',V|Roles], % cas très fréquent
            Env,Options,[':*':pp(p("on"),DSyntR)|Env],Options):-!,
            amr2dsr(['date-entity',V|Roles],_Concept,_POS,DSyntR).
processRole(':time',_OuterConcept,_OuterPOS,AMR,
            EnvIn,OptionsIn,EnvOut,OptionsOut):-
    amr2dsr(AMR,_Concept,_POS,DSyntR),
    processTime(DSyntR,EnvIn,OptionsIn,EnvOut,OptionsOut).
 processTime("*unknown*",Env,Options,Env,[typ({"int":"whn"})|Options]).
 processTime(a("former"),Env,Options,[':&':adv("formerly")|Env],Options).
 processTime(DSyntR,Env,Options,[':*':DSyntR|Env],Options).

processRole(':topic',_OuterConcept,_OuterPOS,AMR,
            Env,Options,[':*':pp(p("about"),DSyntR)|Env],Options):-
    amr2dsr(AMR,_Concept,_POS,DSyntR).

processRole(':wiki',_OuterConcept,_OuterPOS,'-', %ignore when no value
            Env,Options,Env,Options).
processRole(':wiki',_OuterConcept,_OuterPOS,AMR,
            Env,Options,Env,[tag("a",{"href":("https://en.wikipedia.org/wiki/"+Value)})|Options]):-
    amr2dsr(AMR,_Concept,_POS,Value).

%%%%%%% unknown role : add it to the environment
processRole(Role,_OuterConcept,_OuterPOS,AMR,Env,Options,[Role:DSyntR|Env],Options):-
    % writeln("** processRole: unknown role":Role),
    amr2dsr(AMR,_,_,DSyntR).
