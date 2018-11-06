:-encoding(utf8).
:-[utils].
% Parse an AMR into a Semantic Representation along the lines of 
%    Johan Bos,Expressive Power of Abstract Meaning
%    Representation, Computational Linguistics, 42:3, pp 527-535, 2016.

%%% This version checks correctness of the input and outputs error messages that
%%% pinpoints the position of errors.
%%     when the AMR is valid, it creates the same structure as parse.pl
%%  for similar predicate we use the same names as in parse.pl but prefixed by "x"

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

%%% tokenization that keeps track of line or column number

xToks(S,WCs):-atom_chars(S,Chars),phrase(xToks(WCs,(1,1),_),Chars),!.

xToks(Ws,(L,Cin),LCout) --> [' '],!,{Cin1 is Cin+1},xToks(Ws,(L,Cin1),LCout). % ignore space
xToks([(W,L,Cin)|WCs],(L,Cin),LCout) -->
    [C],{char_word(C),Cin1 is Cin+1},!,xWord(Cs,Cin1,Cout0),{atom_chars(W,[C|Cs])},
    xToks(WCs,(L,Cout0),LCout).
xToks([(W,L,Cin)|WCs],(L,Cin),LCout) -->
    [C],{separator(C),Cin1 is Cin+1},!,{atom_chars(W,[C])},
    xToks(WCs,(L,Cin1),LCout).
xToks([(W,L,Cin)|Ws],(L,Cin),LCout) -->
    ['"'],!,xInString(Cs,(L,Cin+1),LCout0),{string_chars(W,Cs)},
    xToks(Ws,LCout0,LCout).
xToks(Ws,(L,_Cin),LCout)--> eol,!,{L1 is L+1},xToks(Ws,(L1,1),LCout).% next line
xToks(Ws,(L,Cin),LCout)    -->% ignore what follows # on the current line 
    [';'],!,xComment(Cin,_),{L1 is L+1},xToks(Ws,(L1,1),LCout). % skip to next line
xToks(Ws,(L,Cin),LCout)    --> [C],!,{Cin1 is Cin+1, % flag other characters
    format('Line ~d:~d : ~w : illegal character~n',[L,Cin,C])},
    xToks(Ws,(L,Cin1),LCout). 
xToks([],(Lin,Cin),(Lout,Cout)) -->[],{Lout is Lin,Cout is Cin}.


xWord([C|Cs],Cin,Cout)-->[C],{char_word(C)},xWord(Cs,Cin+1,Cout).
xWord([],Cin,Cout) -->  [],{Cout is Cin}.

% create a string upto the next '"' (caution: does not deal with embedded " or \")
xInString([],(L,Cin),(L,Cout)) --> ['"'],!,{Cout is Cin+1}.
xInString([],(L,Cin),(L1,1)) --> eol,!,
    {L1 is L+1,format('Line ~d:~d : end of line within a string ~n',[L,Cin])}.
xInString([C|Cs],(L,Cin),LCout) --> [C],xInString(Cs,(L,Cin+1),LCout).
xInString([],(L,Cin),(L,Cin)) --> [],!,
    {format('Line ~d:~d : end of AMR within a string ~n',[L,Cin])}.

% ignore a comment until the of line
xComment(Cin,Cout) --> eol,!,{Cout is Cin}.
xComment(Cin,Cout) --> [_],!,xComment(Cin+1,Cout).
xComment(Cin,Cout) --> [],{Cout is Cin}.

eol --> ['\n'].   % usual
eol --> ['\r','\n']. % just in case
eol --> [C],{string_code(1,C,8233)}. % code for newLine when pasting to the Prolog console

varName(Token):-re_match("^[a-z]+([0-9]+)?$",Token).
conceptName(Token):-re_match("^[-\\w]+(\\d+)?",Token).
roleName(Token):-re_match("^:[-\\w]+",Token).
constant(Token):- re_match("^(\\+|-|(\\+|-)?(\\d+((\\.|:)\\d+)?))$",Token). % +,-, number or time with :

%% check for an expected char or type
%%  if it does not match, go further to try to recover by searching the next matching char/type
checkChar(Char)-->[(Char,_L,_C)],!.
checkChar(Char)--> [(Found,L,C)],
    {format('Line ~d:~d : ~w expected but ~w found~n',[L,C,Char,Found])},!,
    skip,[(Char,_L,_C)].
checkChar(Char)-->[],
    {format('End of AMR encountered when looking for ~w~n',[Char])}.

checkType(Kind,Token)-->[(Token,_L,_C)],{call(Kind,Token)},!.
checkType(Kind,_)--> [(Token,L,C)],
    {format('Line ~d:~d : ~w expected, but found ~q~n',[L,C,Kind,Token])},!,
    skip,[(Token1,_L,_C)],{call(Kind,Token1)}.
checkType(Kind)-->[],
    {format('End of AMR encountered when looking for ~w~n',[Kind])}.

skip --> [].
skip --> [_], skip.

xAmrParse(V,Vars,Vars)             --> [(V,_L,_C)],{constant(V)},!.
xAmrParse(V,Vars,[V|Vars])         --> [(V,_L,_C)],{varName(V)},!.
xAmrParse([Concept,V|Rs],Vin,Vout) -->
    checkChar('('),checkType(varName,V),checkChar('/'),checkType(conceptName,Concept),
    xRoles(Rs,[V|Vin],Vout).

xRoles([],Vin,Vin) --> [(')',_L,_C)],!.
xRoles([Ramr|Ramrs],Vin,Vout) --> xRole(Ramr,Vin,Vin1),!,xRoles(Ramrs,Vin1,Vout).

xRole([RoleName,AMR],Vin,Vout) -->
    checkType(roleName,RoleName),xAmrParse(AMR,Vin,Vout).

amrParseValidate(AMRstring,AMR,Errors):-
    with_output_to(string(Errors),
        (xToks(AMRstring,Toks),
         phrase(xAmrParse(AMR0,[],Vars),Toks,Rest)->
            (Rest=[]->keepRepeatedVars(Vars,RepVars),projectVars(AMR0,RepVars,AMR);
             Rest=[(_,L,C)|_],format('Line ~d:~d : parsing ended prematurely~n',[L,C]));
         format('Missing parenthesis at end of AMR'))).

%%% some good and bad AMR for testing
xAMR(0,'(s / small :domain (m / marble) :polarity -)').
xAMR(1,'(s / + small :domain (m / marble) :polarity -)').
xAMR(2,'(s  :domain (m / marble) :polarity -)').
xAMR(3,'(s / small @ :domain "toto" :polarity -)').
xAMR(4,'(s /small  :domain 
         (m / marble) 1.23 :polarity -)').
xAMR(5,'(s/small :a "toto
            :b d)').

xTestValidateAmrParse(N):-
    xAMR(N,S),format('---~d :~n~s~n',[N,S]),
    amrParseValidate(S,AMR,Errors),
    (Errors=""->pprint(AMR);format('Parsing errors found in AMR~n~w~n',Errors)).

testAllValidateAmrParse:-xAMR(N,_),xTestValidateAmrParse(N),fail.
