:-encoding(utf8).

%%%% pronoun generation
% find the path of variable within an AMR 
getVarPath(Var,[Concept,Var|_],_,[Concept]):-!.
getVarPath(Var,[Concept,_|Roles],PathIn,[Concept|PathOut]):- atom(Concept),getRoleVarPath(Var,Roles,PathIn,PathOut),!.
getVarPath(Var,Var,_,[]):-!.

getRoleVarPath(Var,[[Role,AMR]|_]  ,PathIn,[Role|PathOut])   :- atom(Role),getVarPath(Var,AMR,PathIn,PathOut),!.
getRoleVarPath(Var,[_|Roles]       ,PathIn,PathOut)          :- getRoleVarPath(Var,Roles,PathIn,PathOut).

% find all variable reference within an AMR
getVarRefs([Concept,_Var|Roles],VarsIn,VarsOut):-atom(Concept),getRoleVarRefs(Roles,VarsIn,VarsOut).
getVarRefs(Var,VarsIn,[Var|VarsIn]):-% ignore atom representing numbers, +,- 
    isLegalVarName(Var). 
getVarRefs(_,Vars,Vars).

getRoleVarRefs([],Vars,Vars).
getRoleVarRefs([[_Role,AMR]|Roles],VarsIn,VarsOut):-
    getVarRefs(AMR,VarsIn,VarsOut0),getRoleVarRefs(Roles,VarsOut0,VarsOut).

% access the "global" context 
getPaths(Var,VarPath,ConceptPath):-
    currentAMR(Top,AllVars),memberchk(Var,AllVars),!,
    getVarPath(Var,Top,[],VarPathR),reverse(VarPathR,VarPath),
    re_replace('\\**$'/a,'',Var,VarNoStar),
    getVarPath(\VarNoStar,Top,[],ConceptPathR),reverse(ConceptPathR,ConceptPath).

genRef(Var,Concept,POS,Ref):-
    getPaths(Var,VarPath,ConceptPath),
    ConceptPath=[Concept|_],
    (pronoun(Concept,Pro) -> genRefPro(ConceptPath,VarPath,Pro,Ref),POS='Pronoun'; 
        gender(Concept,G),genRefAux(ConceptPath,VarPath,G,POS,Ref)
    ).

%% a testbench for the "separation" of var references
testSeparate(AMRstring):-
    amrParse(AMRstring,AMR),pprint(AMR),nl,
    elimInv(AMR,AMRnoInv), pprint(AMRnoInv),nl,
    getVarRefs(AMRnoInv,[],VarRefs),writeln('VarRefs':VarRefs),
    separateRefInstances(AMRnoInv,[],AMRnoInvOut,VarsOut),
    pprint(AMRnoInvOut),nl,writeln('VarsOut':VarsOut).

% Choice rules for the pronoun referencing a variable
%  using the table
% VerbC==VerbV
%       ARG0C  ARG1C   ARGiC
% ARG0V  null  myself  I
% ARG1V  me      I     me
% ARGiV  I       me    me
%
% VerbC=/=VerbV
%       :ARG0C  :ARG1C   ARGiC
% :ARG0V   null  I       I
% :ARG1V   me    me      me
% :ARGiV   me    me      me
% mettre la phrase à l'infinitif sans pronom si sujet est null

%%% genRefAux(-ConceptPath,-VarPath,-Gender,+Pronoun)
genRefAux(_,[':poss'|_],G,'Determiner',d("my")*pe("3")*g(G)*n("s")):-!.
% remove reference in the relative introduced by the inverse roles
genRefAux(_,[':ARG0', _Verb, ':*:ARG0'|_], _,'Pronoun',null):-!. % même sujet
genRefAux(_,[':ARG1', _Verb, ':*:ARG1'|_], _,'Pronoun',null):-!. % même COD
% same verb
genRefAux([_,':ARG0',Verb|_],[':ARG0',Verb|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,':ARG1',Verb|_],[':ARG0',Verb|_],G,'Pronoun',pro("myself")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,  ARGi ,Verb|_],[':ARG0',Verb|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-
    re_match('^:ARG\\d$',ARGi),!.
genRefAux([_,':ARG0',Verb|_],[':ARG1',Verb|_],G,'Pronoun',pro("myself")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,':ARG1',Verb|_],[':ARG1',Verb|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,  ARGi ,Verb|_],[':ARG1',Verb|_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGi),!.
genRefAux([_,':ARG0',Verb|_],[  ARGj ,Verb|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGj),!.
genRefAux([_,':ARG1',Verb|_],[  ARGj ,Verb|_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-
    re_match('^:ARG\\d$',ARGj),!.
genRefAux([_,  ARGi ,Verb|_],[  ARGj ,Verb|_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-
    re_match('^:ARG\\d$',ARGi), re_match('^:ARG\\d$',ARGj),!.
% different verb
genRefAux([_,':ARG0'|_],[':ARG0'|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,':ARG1'|_],[':ARG0'|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,  ARGi |_],[':ARG0'|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGi),!.
genRefAux([_,':ARG0'|_],[':ARG1'|_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,':ARG1'|_],[':ARG1'|_],G,'Pronoun',pro("I")*pe("3")*g(G)*n("s")):-!.
genRefAux([_,  ARGi |_],[':ARG1'|_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGi),!.
genRefAux([_,':ARG0'|_],[  ARGj |_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGj),!.
genRefAux([_,':ARG1'|_],[  ARGj |_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-re_match('^:ARG\\d$',ARGj),!.
genRefAux([_,  ARGi |_],[  ARGj |_],G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")):-
    re_match('^:ARG\\d$',ARGi), re_match('^:ARG\\d$',ARGj),!.
% by default
genRefAux(_,_,G,'Pronoun',pro("me")*pe("3")*g(G)*n("s")).

% reference to an existing pronoun
genRefPro(_,[':ARG1'|_],pro(_)*pe(P)*g(G)*n(N),pro("me")*pe(P)*g(G)*n(N)).
genRefPro(_,_,Pro,Pro).
