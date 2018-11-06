% adapté de la première réponse à https://dtai.cs.kuleuven.be/projects/ALP/newsletter/archive_93_96/net/strings/RE.html

%%% tentative d'éliminer les regexp pour les remplacer par des dcg
%%    si le matching fonctionne assez bien, les remplacements sont beaucoup plus problématiques
%%    je suis donc revenu à la version avec regexp

:- op(500, xf, *).
NT* --> [] | NT, NT* .
:- op(500, xf, +).
NT+ --> NT, NT* .
:- op(500, xf, ?).
NT? -->[] |  NT.

any    --> [_].
letter --> [C],{char_type(C,alpha)}.
digit  --> [C],{char_type(C,digit)}.
rest   --> [C],{char_type(C,alnum)};['-'].

plusMinus --> ['+']|['-'].

%% define useful predicates for the application
isVariable(S)    :-dcgMatch((letter,rest*),S).
isConcept(S)     :-dcgMatch((rest+,digit*),S).
isRole(S)        :-dcgMatch(([':'],rest+),S).
isArg(S)         :-dcgMatch(([':','A','R','G'],digit),S).
isArgOp(S)       :-dcgMatch(([:],(['A','R','G']|['o','p']),digit),S).
isLegalVarName(S):-atom(S),dcgMatch((letter,letter?,digit?,['*']*),S).
isConstant(S)    :-dcgMatch(plusMinus|(plusMinus?,digit+,((['.']|[':']),digit+)?),S).

%%%%%%%%  MATCHING
% match the whole string or atom S with Exp a regular expression as a DCG
dcgMatch(Exp,S):-string(S),!,string_chars(S,Cs),phrase(Exp,Cs,[]).
dcgMatch(Exp,S):-atom(S),  !,atom_chars(S,Cs)  ,phrase(Exp,Cs,[]).

%%%%%%%% REPLACEMENT
% replace From (an atom) to To (an atom or the empty string) in In (a string or and atom)
% if In is an atom then the Out will be an an atom, if In is a string, then Out will be a string 
replace(From,To,In,Out):-string(In),!,string_chars(In,InCs),replaceChars(From,To,InCs,OutCs),string_chars(Out,OutCs).
replace(From,To,In,Out):-atom(In),!,atom_chars(In,InCs),replaceChars(From,To,InCs,OutCs),atom_chars(Out,OutCs).

replaceChars(From,To,InCs,OutCs):-atomic(From),
    (atom_length(From,1)->
       (atom_length(To,0)->delete(InCs,From,OutCs);
                           replaceCharsC(From,To,InCs,OutCs));
     atom_chars(From,FromCs),atom_chars(To,ToCs),replaceStringC(FromCs,ToCs,InCs,OutCs)).

replaceCharsC(_From,_To,[],[]).
replaceCharsC(From,To,[From|Ins],[To|Outs]):-replaceCharsC(From,To,Ins,Outs).
replaceCharsC(From,To,[X|Ins],[X|Outs]):-replaceCharsC(From,To,Ins,Outs).

replaceStringC(_From,_To,[],[]).
replaceStringC(From,To,Ins,Outs):-append(From,Rest,Ins),append(To,Rest,Ins0),!,replaceStringC(From,To,Ins0,Outs).
replaceStringC(From,To,[X|Ins],[X|Outs]):-replaceStringC(From,To,Ins,Outs).

% CAUTION:
%   From should be a regex 
%   To   should be list of "chars", in particular [] for the empty string
replaceRE(From,To,In,Out):-string(In),!,string_chars(In,InCs),replaceReC(From,To,InCs,OutCs),string_chars(Out,OutCs).
replaceRE(From,To,In,Out):-atom(In),!,atom_chars(In,InCs),replaceReC(From,To,InCs,OutCs),atom_chars(Out,OutCs).

replaceReC(_From,_To,[],[]).
replaceReC(From,To,Ins,Outs):-phrase(From,Ins,Rest),append(To,Rest,Ins0),replaceReC(From,To,Ins0,Outs).
replaceReC(From,To,[X|Ins],[X|Outs]):-replaceReC(From,To,Ins,Outs).


