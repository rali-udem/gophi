:-encoding(utf8).

% Parse an AMR into a Semantic Representation along the lines of 
%    Johan Bos,Expressive Power of Abstract Meaning
%    Representation, Computational Linguistics, 42:3, pp 527-535, 2016.
%
%% creates a Semantic Representation of the form
%%        [concept, instance variable, [RoleName0, amr0], [RoleName1, amr1] ...]
%%     or var
%% for example:
%       (e/give-01  # a comment to ignore
%                    :ARG2 (y/child)
%                    :ARG0 (x/person :named "Ms Ribble")
%                    :ARG1 (z/envelope))
%% is parsed as
%%       [give-01,e,[:ARG2,[child,y]],[:ARG0,[person,x,[:named,Ms Ribble]]],[:ARG1,[envelope,z]]]
%% and shown in indented form as
%        'give-01',e,
%                  [':ARG2',[child,y]],
%                  [':ARG0',[person,x,
%                                     [':named',"Ms Ribble"]]],
%                  [':ARG1',[envelope,z]]]

%%% simple tokenization (does not keep track of line or column number)

% tokenize a string representing an AMR into a list of tokens
toks(S,Ws):-atom_chars(S,Cs),phrase(toks(Ws),Cs),!.
toks([W|Ws]) -->
    [C],{char_word(C)},!,word(Cs),{atom_chars(W,[C|Cs])},
    toks(Ws).
toks([W|Ws]) -->
    [C],{separator(C)},!,{atom_chars(W,[C])},
    toks(Ws).
toks([W|Ws]) -->
    ['"'],!,inString(Cs),{string_chars(W,Cs)},
    toks(Ws).
toks(Ws) --> ([';'];['#']),!,comment,toks(Ws). % ignore what follows ; or # on the current line
toks(Ws) --> [_],toks(Ws). % ignore other characters
toks([]) --> [].

word([C|Cs])-->[C],{char_word(C)},word(Cs).
word([]) -->  [].

% create a string upto the next '"' (caution: does not deal with embedded " or \")
inString([]) --> ['"'],!.
inString([C|Cs]) --> [C],inString(Cs).
% ignore a comment until the of line
comment -->['\n'],!.
comment --> [_],!,comment.
comment -->[].

% DCG for parsing a list of tokens
% according to Definition 1
% 2nd and 3rd parameters gather variable names
amrParse([Concept,V|Rs],Vin,Vout) -->
    ['(',V,'/',Concept],!,roles(Rs,[V|Vin],Vout).
amrParse(V,Vars,Vars)    --> [V],{string(V)}. % do not consider "strings" as variable
amrParse(V,Vars,[V|Vars])--> [V].

roles([],Vin,Vin) --> [')'],!.
roles([R|Rs],Vin,Vout) --> role(R,Vin,Vin1),roles(Rs,Vin1,Vout).

role([R,AMR],Vin,Vout)--> [R], amrParse(AMR,Vin,Vout).

%%%% simple parsing without any error recovery nor error message
amrParse(AMRstring,AMR):-
    toks(AMRstring,Toks),!,
    phrase(amrParse(AMR0,[],Vars),Toks),!,
    keepRepeatedVars(Vars,RepVars),!,
    projectVars(AMR0,RepVars,AMR).

%%% unit testing
:-[utils].
:-[baselineGen].
:-[examples].
:-[inverseRoles].

testAmrParse(N):-
    ex(N,S,Sent),format('~s~s~s~s~n~s~n',['--- ',N,':',Sent,S]),
    amrParse(S,AMR),
    write(AMR),nl,
    pprint(AMR),nl,
    baseGen(AMR,Out,[]),writeWords(Out),nl.

testAllAmrParse:-ex(N,_),testAmrParse(N),nl,fail.

%  test of variable projection
testProj5a(Struct):-
    projectVars(
        ['dry-01',e,[':ARG0',[person,x,[':named',"Mr Krupp"]]],[':ARG1',x]],
        [x],Struct).

% test of variable elimination
testElim(N):-
    ex(N,S,Sent),format('~s~s~s~s~n~s~n',['--- ',N,':',Sent,S]),
    amrParse(S,AMR),
    pprint(AMR),nl,
    % trace,
    elimInv(AMR,AMR1),
    pprint(AMR1),nl.

exOf(['2a','2b','9a','SemEval-3','SemEval-5','Tut-03']).
testAllOf:-
    forall((exOf(Exs),member(Ex,Exs)),testElim(Ex)).


