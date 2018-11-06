%% DCG rules for transforming a Deep Syntactic Representation into a Surface Syntactic Representation
%%   - remove null before transformation
%%   - list elements are indented with (newlines and spaces)
%%  dsr2jsReal(DSR,SSR)
%%         where SSR is a list of string
%%              that can be written with maplist(write,SSR)
%%                    or concatenated with atomic_list_concat(SSR,Out)

dsr2jsReal(DSR)-->{deleteNull(DSR,DSRnonull)},
        ({DSRnonull=..[ls|Rest],SDSR=..[s|Rest]}->dsr2jsReal(SDSR,0); %add a top S in case of a top ls
         {DSRnonull=DSRnonull0*Options,DSRnonull0=..[ls|Rest],SDSR=..[s|Rest]}
                               -> dsr2jsReal(SDSR*Options,0);
         dsr2jsReal(DSRnonull,0)).

dsr2jsReal(ls,_N)--> !,['Q("")']. % if deleteNull produced a lone ls
% dsr2jsReal(c,_N) --> !,['Q("")']. % if deleteNull produced a lone c
dsr2jsReal(null,_N) --> !,['Q("")']. % if deleteNull produced a lone null
dsr2jsReal(X,_N)-->{string(X),!,quote(X,QX)},[QX].
dsr2jsReal([],_)-->!.                     % caution atomic([]) succeeds
dsr2jsReal(X,_N)-->{atomic(X)},!,[X]. 
% operators
dsr2jsReal(+X,N)-->{X=..[Pred,Val],call(Pred,Val,Val1)},dsr2jsReal(Val1,N). % evaluate a two argument predicate
dsr2jsReal(($)/X,N)-->!,dsr2jsReal(X,N).
dsr2jsReal($(X)/_,N)-->!,dsr2jsReal(X,N).
dsr2jsReal(_/X,N)-->!,dsr2jsReal(X,N).
dsr2jsReal(q(q(X)),N)-->!,dsr2jsReal(q(X),N).
dsr2jsReal(X+q(Y),_N)-->{atomic(X),!,atomics_to_string([X,Y],Z),quote(Z,QZ)},[QZ].
dsr2jsReal(q(X)+Y,_N)-->{atomic(Y),!,atomics_to_string([X,Y],Z),quote(Z,QZ)},[QZ].
dsr2jsReal(X+Y,_N)-->{atomic(X),atomic(Y),!,atomics_to_string([X,Y],Z),quote(Z,QZ)},!,[QZ].
dsr2jsReal(X+q(Y),N)-->{mergeQ(X+q(Y),Z)},!,dsr2jsReal(q(Z),N).
dsr2jsReal(X+Y,N) --> % serialise Y even though this might create strange string
    {phrase(dsr2jsReal(Y,N),Ys0),
     atomics_to_string(Ys0,Ys),atomics_to_string([X,Ys],Z),quote(Z,QZ)},[QZ].
dsr2jsReal({X},N)-->!,['{'],{N1 is N+1},dsr2jsReal(X,N1),['}'].
dsr2jsReal(X:Y,N)-->!,dsr2jsReal(X,N),[':'],dsr2jsReal(Y,N).
dsr2jsReal(_^X,N)-->!,dsr2jsReal(X,N). % should never happen, but...

dsr2jsReal(X*Option,N)-->{string(X)},!,dsr2jsReal(q(X)*Option,N).
dsr2jsReal(ls(X)*Option,N)-->{string(X)},!,dsr2jsReal(q(X)*Option,N).
dsr2jsReal(X*Option,N)-->!,
   ({X=..[ls,S|Xs],S=..[s|Ss]} -> %% merge arguments of ls(s(Ss..)Xs..)*Option => s(Ss..,Xs..)*Option
              {append(Ss,Xs,Args),NewS=..[s|Args]}, dsr2jsReal(NewS,N);
    dsr2jsReal(X,N)),    %% normal case
    {Option=..[C|Vals],opt(C),string_length(C,L)},['.',C,'('],dsr2jsReal(Vals,N+L+2),[')'].

dsr2jsReal([X],N)   -->!,dsr2jsReal(X,N).
dsr2jsReal([ls|Xs],N)--> dsr2jsReal(Xs,N).
dsr2jsReal([X|Xs],N)-->
    dsr2jsReal(X,N),[',\n'],indent(N),dsr2jsReal(Xs,N).
dsr2jsReal(S,N)-->
    {S=..[P,Arg|Args]},
    ({terminal(P,Pout)} -> [Pout,'('],dsr2jsReal(Arg),[')'] ;
     {P=ls} -> dsr2jsReal([Arg|Args],N);
     % {P=t}  -> dsr2jsReal(Arg,N);
     {syntagme(P,Pout)},[Pout,'('],{string_length(P,Pl),N1 is N+Pl+1},dsr2jsReal([Arg|Args],N1),[')']).

% create quoted string
quote(S,QS):-format(string(QS),'~q',S).

% merge q(X0) +...+q(Xn) s into a single q(X0+...+Xn)
mergeQ(q(X0)+q(X1),X0X1):-!,atomics_to_string([X0,X1],' ',X0X1).
mergeQ(Xs+q(X1),XsX1):-mergeQ(Xs,XsM),atomics_to_string([XsM,X1],' ',XsX1).

% remove null elements
isNull(null).
isNull(null/_).
isNull(null*_).

deleteNull(X,X):-atomic(X),!.
deleteNull(null+X,Out):-deleteNull(X,Out).
deleteNull(X+null,Out):-deleteNull(X,Out).
deleteNull(X*Option,Out):-deleteNull(X,X1),(X1=ls->Out=X1;Out=X1*Option).
deleteNull([X|Xs],Xs1):-isNull(X),!,deleteNull(Xs,Xs1).
deleteNull([X|Xs],[X1|Xs1]):-deleteNull(X,X1),!,deleteNull(Xs,Xs1).
deleteNull(X,X1) :- X=..[P|Args],deleteNull(Args,Args1),X1=..[P|Args1].

%  Translation table for jsReal symbols
%  http://rali.iro.umontreal.ca/JSrealB/documentation/user.html
terminal(n,'N').
terminal(a,'A').
terminal(pro,'Pro').
terminal(d,'D').
terminal(v,'V').
terminal(adv,'Adv').
terminal(p,'P').
terminal(c,'C').
terminal(q,'Q').
terminal(no,'NO').

syntagme(s,'S').
syntagme(np,'NP').
syntagme(ap,'AP').
syntagme(advp,'AdvP').
syntagme(vp,'VP').
syntagme(pp,'PP').
syntagme(sp,'SP').
syntagme(cp,'CP').

opt(X):-memberchk(X,['a','b','en','c','t','pe','n','g','f','typ','tag','pro','nat','ord','add']),!.
opt(X):-format('** ~w: unknown option~n',X).
