:- encoding(utf8).

% replace - by spaces and remove trailing numbers 
cleanConcept(Concept,CleanConcept):-
    re_replace('-'/g,' ',Concept,Concept1),re_replace(' \\d\\d| yy','',Concept1,CleanConcept).

adj2adv(a(Adj0),adv(Adv0)):-
    % remove possible trailing number
    re_matchsub('([a-z]+)(-\\d+)?$',Adj0,Sub,[capture_type(atom)]),atom_string(Adj,Sub.1),!,
    %% rules from http://www.ef.com/english-resources/english-grammar/forming-adverbs-adjectives/
    (member(Adj,['early','fast','hard','high','late','near','straight','wrong'])->Adv=Adj;
     atom_concat(S,'y',Adj)->atom_concat(S,'ily',Adv);
     atom_concat(S,'le',Adj)->atom_concat(S,'ly',Adv);
     Adj='public'->Adv='publicly';
     atom_concat(S,'ic',Adj)->atom_concat(S,'ily',Adv);
     atom_concat(Adj,'ly',Adv)
    ),atom_string(Adv,Adv0).
adj2adv(Ls,Ls1):- % deal with embedded adjective in a ls
    Ls=..[ls|Args],maplist(adj2adv,Args,Args1),Ls1=..[ls|Args1].
adj2adv(Adj0,Adj0).

% remove dictionary entry
delete(POS,Key):-
    Rem=..[POS,Key,_],
    retract(Rem).
% change dictionary entry
patch(POS,Key,Val):-
    (delete(POS,Key)->true;true), 
    Add=..[POS,Key,Val],
    assert(Add).

capitalize(Atom,String):-
    atom_string(Atom,S),string_chars(S,[C|Cs]),
    upcase_atom(C,CU),string_chars(String,[CU|Cs]).

%%% get POS of a concept
getConceptPos(Concept,'Special',ConceptPred):-specialConcept(Concept,ConceptPred).
getConceptPos(Concept,'Verb',DSyntR):-verb(Concept,DSyntR),!.
getConceptPos(Concept,'Noun',DSyntR):-noun(Concept,DSyntR),!.
getConceptPos(Concept,'Adjective',DSyntR):-adjective(Concept,DSyntR),!.
getConceptPos(Concept,'Conjunction',DSyntR):-conjunction(Concept,DSyntR),!.
getConceptPos(Concept,'Pronoun',DSyntR):-pronoun(Concept,DSyntR),!.
getConceptPos(Concept,'Adverb',DSyntR):-adverb(Concept,DSyntR),!.
getConceptPos(Concept,'Preposition',DSyntR):-preposition(Concept,DSyntR),!.
getConceptPos(Concept,'Determiner',DSyntR):-determiner(Concept,DSyntR).
getConceptPos(Concept,'UnknownPOS',q(String)):- 
    writeln('**unknown concept:':Concept),cleanConcept(Concept,String). 

%%%% show info about words from the dictionary
%%  the search string is an anchored regex 

showPOS(POS,W):-
    atom_concat('^',W,W1),  % re_option anchored(true) having no effect,we add ^ in front 
    bagof((Z,X),(call(POS,Z,X),re_match(W1,Z)),Ys)   % get all matching words and their DSyntR
    -> (write('**'),write(POS),nl,
        forall(member((Z,X),Ys),
               (write(Z),write(':'),numbervars(X),writeq(X),nl)));
    true.

showMorphVerb(W):-
    atom_concat('^',W,W1),
    bagof((X,NV,AV),(morphVerb(X,NV,AV),re_match(W1,X)),Ys),!,
    writeln('**MorphVerb '),
    forall(member((X,NV,AV),Ys),
           (write(X:""),
            (NV\=null -> write(" Noun":NV);true),
            (AV\=null -> write(" Actor":AV);true),nl)).
showMorphVerb(_).
    
showVerb(W):-
    atom_concat('^',W,W1),
    (bagof((X,Verb),(verbalization(X,Verb),re_match(W1,X)),Ys)
      -> writeln('**Verbalisations-simple'),
         forall(member((X,Verb),Ys),(writeln(X:Verb)));
      true),
    (bagof((X,Role,Arg,Verb),
      (verbalization(X,Role,Arg,Verb),re_match(W1,X)),Ys)
      -> writeln('**Verbalisations-complex'),
         forall(member((X,Role,Arg,Verb),Ys),(writeln(X:Role:Arg:Verb)));
      true),!.
showVerb(_).
    
    
%% show POS in the same order as for getConceptPos(...) 
info(W):-
    forall(member(POS,['verb','noun','adjective','conjunction',
                       'pronoun','adverb','preposition','determiner']),
           showPOS(POS,W)),
    showMorphVerb(W),showVerb(W).
