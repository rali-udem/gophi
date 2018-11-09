:-encoding(utf8).
%%% utility predicates 

reduce(Arg^Expr,Arg,Expr). % one-level beta-reduction

char_word(C):-char_type(C,alnum).% alphanumeric
char_word('-').
char_word('+').
char_word(':').
char_word('.').

separator('/').
separator('\\').
separator('(').
separator(')').

%%  check if atom is a legal variable name (atom composed of one or two lower case letters 
%%       optionaly followed by a digit and terminated by 0 or more *
isLegalVarName(Var):-
    atom(Var),re_match('^[a-z][a-z]?[0-9]?\\**$',Var).

%%  Variable projection
keepRepeatedVars([],[]).
keepRepeatedVars([X|Xs],[X|Xs1]):-member(X,Xs),!,keepRepeatedVars(Xs,Xs1).
keepRepeatedVars([_|Xs],Xs1):-keepRepeatedVars(Xs,Xs1).

hasRepetition([X|Xs]):-memberchk(X,Xs).
hasRepetition([_|Xs]):-hasRepetition(Xs).

% project variables and adding \ in front
projectVars(Px,_,Px):-
    atomic(Px),!.
projectVars([C,X|Roles],Vars,[C,\X|Projs]):- % projeter
    atomic(X),member(X,Vars),!,
    projectVarsRoles(Roles,Vars,Projs).
projectVars([C,X|Roles],Vars,[C,X|Projs]):-
    projectVarsRoles(Roles,Vars,Projs).

projectVarsRoles([],_,[]).
projectVarsRoles([[Ri,Ai]|Roles],Vars,[[Ri,Proj0]|Projs]):-
    projectVars(Ai,Vars,Proj0),
    projectVarsRoles(Roles,Vars,Projs).

%% number multiple variable references
%% as it is important to distinguish different occurrences of reference to the same variable
%% add *s to the references after the first one
%%     separateRefInstances(AMRin, varsIn, AMRout, varsOut)
separateRefInstances(Var,UsedVarsIn,NewVar,UsedVarsOut):-
    atomic(Var),
    (isLegalVarName(Var)->separateVars(Var,UsedVarsIn,NewVar,UsedVarsOut);
                        NewVar=Var,UsedVarsOut=UsedVarsIn).
    
separateRefInstances([C,X|Roles],UsedVars,[C,X|RolesOut],UsedVarsOut):-
    separateRefInstancesRoles(Roles,UsedVars,RolesOut,UsedVarsOut).

separateRefInstancesRoles([],Vars,[],Vars).
separateRefInstancesRoles([[Ri,Ai]|Roles],UsedVarsIn,[[Ri,AiOut]|RolesOut],UsedVarsOut):-
    separateRefInstances(Ai,UsedVarsIn,AiOut,UsedVarsOut0),
    separateRefInstancesRoles(Roles,UsedVarsOut0,RolesOut,UsedVarsOut).

separateVars(Var,VarsIn,NewVar,[NewVar|VarsIn]):-
    member(Var,VarsIn),nextInstanceName(Var,VarsIn,NewVar).
separateVars(Var,VarsIn,Var,[Var|VarsIn]).

nextInstanceName(Var,[Var0|_Vars],NewVar):-
    atom_concat(Var,Stars,Var0),atomic_list_concat([Var,Stars,'*'],NewVar).
nextInstanceName(Var,[_|Vars],NewVar):-
    nextInstanceName(Var,Vars,NewVar).

%%%%%%%%

pprint(T):-pprint(T,0).
pprint(T,N):-phrase(pp(T,N),PP),atomic_list_concat(PP,S),write(S),nl.

% create an atom of N spaces... must force evaluation of N which format does not do...
indent(N) --> {N1 is N,format(atom(S),'~*+~w',[N1,''])},[S].
% output a quoted term
quote(T) --> {with_output_to(atom(QT),writeq(T))},[QT].

%% DCG for pretty-printing a term
pp(T) --> pp(T,0),['\n'].

pp(T,_)      --> {atomic(T)},!,quote(T).
pp(T,_)      --> {var(T)},!,[T].
pp(\X,_)     --> !,['\\'],[X].
pp(X:Y,N)    --> !,pp(X,N),[':'],quote(Y).
pp(X*Y,N)    --> !,pp(X,N),['*'],quote(Y).
pp(X+Y,N)    --> !,pp(X,N),['+'],pp(Y,N).
pp($(null)/Y,N)--> !,['$(null)/'],pp(Y,N+8).
pp($(_X)/Y,N)--> !,['$(non-null)/'],pp(Y,N+12).
pp(null/Y,N) --> !,['null/'],pp(Y,N+5).
pp(_X/Y,N) --> !,['non-null/'],pp(Y,N+9).
pp({X},N)    --> !,['{'],pp(X,N+1),['}'].
pp(X^Y,N)    --> !,['λ'],
    pp(X,N+1),['.\n'],{N1 is N+2},indent(N1),pp(Y,N1).
pp([P,Arg1|Args],N) -->!,
    ['['],pp(P,N+1),[','],
    {write_length(P,L,[quoted(true)]),N1 is N+L+2},
    pp(Arg1,N1),!,ppRest(Args,N1),[']'].
pp(X,N)      --> !,
    {X=..[P,Arg1|Args]},
    pp(P,N),['('],
    {write_length(P,L,[quoted(true)]),N1 is N+L+1},
    pp(Arg1,N1),!,ppRest(Args,N1),[')'].

ppRest([],_)-->[].
ppRest([Arg|Args],N)-->[',\n'],indent(N),pp(Arg,N),ppRest(Args,N).
writeWords(Ws):-atomic_list_concat(Ws,' ',Out),write(Out),write('.').

% output semantics using the notation of Bos

:- op(900,yfx,>).         % implication
:- op(800,yfx,&).         % conjunction
:- op(750, fy,~).         % negation

showSem(E):-phrase(shSem(E),Xs),atomic_list_concat(Xs,X),write(X).
%% DCG for outputing an expression
shSem(exists(V,X))-->!,['∃',V,'('],shSem(X),[')'].
shSem(X & Y)      -->!,shSem(X),['∧'],shSem(Y).
shSem(~X)         -->!,['¬'],shSem(X).
shSem([P,X|Xs])   -->!,[P,'('],shSem(X),shSemRest(Xs),[')'].
shSem(X)          -->{string(X)},!,['"',X,'"'].
shSem(X)          -->[X].

shSemRest([])     -->[].
shSemRest([X|Xs]) -->[,],shSem(X),shSemRest(Xs).

% prettyprint the Bos notation
pprintSem(E):-phrase(ppSem(E),Xs),atomic_list_concat(Xs,X),write(X).
%% DCG for outputing an indented expression
ppSem(E) --> ppSem(E,0).
ppSem(exists(V,X),N) --> !,['∃',V,'('],{atom_length(V,L)},ppSem(X,N+L+2),[')'].
ppSem(X & Y,N)       --> !,ppSem(X,N),[' ∧\n'],indent(N),ppSem(Y,N).
ppSem(~X,N)          --> !,['¬'],ppSem(X,N+1).
ppSem([P,X,Y],_N)    --> !,[P,'(',X,',',Y,')']. % special case (probably the only one!)
ppSem([P,X|Xs],N)    --> !,[P,'('],{atom_length(P,L)},ppSem(X,N+1),ppRestSem(Xs,N+L+1),[')'].
ppSem(X,_)           --> {string(X)},!,['"',X,'"'].
ppSem(X,_)           --> [X].

ppRestSem([],_N)     --> [].
ppRestSem([X|Xs],N)  --> [',\n'],indent(N),ppSem(X,N),ppRestSem(Xs,N).


