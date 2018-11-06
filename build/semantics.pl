:-encoding(utf8).
:-['utils'].

% Parse and compute semantics according to the methodology of
%    Johan Bos,Expressive Power of Abstract Meaning
%    Representation, Computational Linguistics, 42:3, pp 527-535, 2016.
%
%     Deal with recurrent variables (section 4)
%
% assertive semantics (def 7)
assertSem([_,\X|_],Phi,PhiX):-!,reduce(Phi,X,PhiX).                % 7.6, 7.7 et 7.8
assertSem(C,Phi,PhiC) :- atomic(C),!, reduce(Phi,C,PhiC).          % 7.1 et 7.2
% assertSem(C:_,Phi,PhiC) :- atomic(C),!, reduce(Phi,C,PhiC).      % 7.2 for variable references
assertSem([P,X],Phi,exists(X, [P,X] &  PhiX)):-                    % 7.3
    !, reduce(Phi,X,PhiX).
assertSem([C,X|Roles],Phi,~Sem) :- % negative polarity             % 7.5
    member([':polarity',"-"],Roles),!,delete(Roles,[':polarity',"-"],Roles1),
    assertSem([C,X|Roles1],Phi,Sem).
assertSem([C,X|Roles],Phi,exists(X, [C,X] & SemRoles & PhiX)) :-   % 7.4
    assertSemRoles(X,Roles,SemRoles),
    reduce(Phi,X,PhiX).

assertSemRole(X,[Rn,An],Sem):-
    assertSem(An,Y^[Rn,X,Y],Sem0),
    elimTrue(Sem0,Sem).

assertSemRoles(X,[RnAn],Sem) :-!,assertSemRole(X,RnAn,Sem).
assertSemRoles(X,[RiAi|Rs],Sem & Sems):-
    assertSemRole(X,RiAi,Sem),
    assertSemRoles(X,Rs,Sems).

testAssertSem(Ex):-ex(Ex,S),amrParse(S,AMR),pprint(AMR),nl,
    assertSem(AMR,_^true,Sem0),
    elimTrue(Sem0,Sem),
    showSem(Sem),!.

elimTrue(true & X,Y):-!,elimTrue(X,Y).
elimTrue(X & true,Y):-!,elimTrue(X,Y).
elimTrue(~X,~Y):-!,elimTrue(X,Y).
elimTrue(Px,Y):-
    Px =..[R,V|Roles],!,
    maplist(elimTrue,Roles,Roles1),
    Y =.. [R,V|Roles1].
elimTrue(X,X).

% projective semantics (def 8)
projSem(C,P^P):- atomic(C),!.            % 8.1
projSem(C:_,P^P):- atomic(C),!.          % 8.2 variable references
projSem([R,\X|Roles],P^P1):-             % 8.6, 8.7 et 8.8
    assertSem([R,X|Roles],X^P,P1).
projSem([_,_],P^P):-!.                   % 8.3
projSem([_,_|Roles],P^P1):-              % 8.4 et 8.5
    projSemRoles(P,Roles,P1).

projSemRole(P,[_,Ai],P1):-!,
    projSem(Ai,P0),
    reduce(P0,P,P1).
projSemRoles(P,[RnAn],P1):-!,projSemRole(P,RnAn,P1).
projSemRoles(P,[RiAi|Rs],P1):-
    projSemRoles(P,Rs,P0),
    projSemRole(P0,RiAi,P1).

testProjSem(Ex):-ex(Ex,S),amrParse(S,AMR),pprint(AMR),nl,
    projSem(AMR,Sem),
    showSem(Sem),!.

combineSem(AMR,Trace,Sem):-
    assertSem(AMR,_^true,AssertSem0),
    elimTrue(AssertSem0,AssertSem),
    (Trace=true -> (write('AssertSem:'),write(AssertSem),nl);true),
    projSem(AMR,ProjSem),
    (Trace=true -> (write('ProjSem:'),write(ProjSem),nl);true),
    reduce(ProjSem,AssertSem,Sem).

%% some unit parsing
:-['parse'].
:-['examples'].

testCombineSem(Ex):-testCombineSem(Ex,false).
testCombineSem(Ex,Trace):-
    ex(Ex,S),amrParse(S,AMR),pprint(AMR),nl,
    combineSem(AMR,Trace,Sem),
    write(Sem),nl,
    pprint(Sem),nl,
    showSem(Sem),nl,
    pprintSem(Sem),nl,!.

testAllCombineSem:-
    ex(N,S,Sent),format('~s~s~s~s~n~s~n',['--- ',N,':',Sent,S]),
    testCombineSem(N),fail.

