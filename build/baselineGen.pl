:- encoding(utf8).

% baseline generator for an AMR

baseGen(T) -->  {atomic(T)},!,
    ({isLegalVarName(T)} -> [] ; [T]). % ignore variables (single letter possibly followed by digits)
baseGen(\_) --> [].
baseGen([C,_V|Roles]) -->
    baseGenFirst(Roles,Roles1),
    {conceptWord(C,W)},[W],
    baseGenRoles(Roles1).

baseGenFirst([],[])--> [].
baseGenFirst([[Role|AMR]|Rs],Rs)-->{member(Role,[':ARG0',':ARG1',':op1'])},!,baseGenRole([Role|AMR]).
baseGenFirst([R|Rs],[R|Rs1])--> baseGenFirst(Rs,Rs1).

baseGenRole([_Role,AMR])-->baseGen(AMR).

baseGenRoles([])--> [].
baseGenRoles([R|Rs]) --> baseGenRole(R),baseGenRoles(Rs).

conceptWord(C,W):-atomic_list_concat([W|_],'-',C).% keep only what precedes "-"
