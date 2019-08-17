:-encoding(utf8).

:-[roles].
:-[pronounReference].
:-[specialConcept].

%%% parse an AMR Semantic Structure to create a Deep Syntactic Structure
%%    an environment of values is built from the structure
%%    in the context of which the lambda structure is then evaluated

amr2dsr([Concept,Ivar|Roles],ConceptOut,POSOut,OutDSyntR):-
    getConceptPos(Concept,POS,ConceptDSyntR),
    processConcept(POS,[Concept,Ivar|Roles],ConceptDSyntR,ConceptOut,POSOut,OutDSyntR).
amr2dsr(Var,Concept,POS,Ref):-                           %% variable reference
    isLegalVarName(Var),genRef(Var,Concept,POS,Ref),!.
amr2dsr(X,OutDSyntR,'Text',q(OutDSyntR)):-               %%  string value
    atomic(X),!,atom_string(X,OutDSyntR).
amr2dsr(X,"*unrecognized*",'UnrecognizedPOS',"*unrecognized*"):-  %% should never happen...
    writeln('+++: unrecognized AMR':X).

processConcept('Special',[Concept,_Ivar|Roles],ConceptDSR,Concept,'Special',OutDSyntR):-
    call(ConceptDSR,Roles,OutDSyntR).
processConcept(_,[Concept,Ivar|Roles],_ConceptDSyntR,ConceptV,POSV,OutDSyntR):-
    getNominalization([Concept,Ivar|Roles],AMRverb),
    amr2dsr(AMRverb,ConceptV,POSV,OutDSyntR),!.
processConcept('Verb',[Concept,_Ivar|Roles],ConceptDSyntR,Concept,'Verb',OutDSyntR):-
    checkPassive(Concept,Roles,ConceptDSyntR,Options0),
    buildRoleEnvOption(Concept,'Verb',Roles,[],Options0,Env,Options1),
    checkSpecial(Roles,Env,EnvOut0,Options1,Options2), % HACK for imperative or yes-or-no question
    checkAccusativePronoun(EnvOut0,EnvOut1), % HACK for possible accusative pronoun as :ARG1 with :ARG0
    processRest(ConceptDSyntR,EnvOut1,Options2,OutDSyntR).
processConcept('Adjective',[Concept,_Ivar|Roles],ConceptDSyntR,Concept,'Adjective',OutDSyntR):-
    buildRoleEnvOption(Concept,'Adjective',Roles,[],[],Env,Options),
    checkAdjectiveArgs(Env,ConceptDSyntR,AdjStruct),% adjective with :ARG0 or :ARG1
    processRest(AdjStruct,Env,Options,OutDSyntR).
processConcept(POS,[Concept,_Ivar|Roles],ConceptDSyntR,Concept,POS,OutDSyntR):-
    POS\='Special',  % we must fail in this case in order to try another concept
    buildRoleEnvOption(Concept,POS,Roles,[],[],Env,Options),
    processRest(ConceptDSyntR,Env,Options,OutDSyntR).

buildRoleEnvOption(_,_,[],Env,Options,Env,Options).
buildRoleEnvOption(OuterConcept,OuterPOS,[[Role,AMR]|RAMRs],EnvIn,OptionsIn,EnvOut,OptionsOut):-
    processRole(Role,OuterConcept,OuterPOS,AMR,EnvIn,OptionsIn,EnvOut0,OptionsOut0),
    buildRoleEnvOption(OuterConcept,OuterPOS,RAMRs,EnvOut0,OptionsOut0,EnvOut,OptionsOut).

% apply the roles of the lambda and add rest of arguments and options 
processRest(DSyntR,Env,Options,OutDSyntR):-
    applyEnv(DSyntR,Env,EnvOut,OutDSyntR0),
    addRestRoles(OutDSyntR0,EnvOut,OutDSyntR1),
    addOptions(Options,OutDSyntR1,OutDSyntR).

checkAdjectiveArgs(Env,DSyntR,Struct):-
    getAllKeys(Env,Keys),findAdjStruct(Keys,DSyntR,Struct).
checkAdjectiveArgs(_Env,DSyntR,DSyntR). % keep as is
findAdjStruct(Keys,DSyntR,(':ARG0':A0)^(':ARG1':A1)^s(A0,vp(v("be"),DSyntR,A1/pp(p("for"),A1)))):-
    memberchk(':ARG0',Keys),!.
findAdjStruct(Keys,DSyntR,(':ARG1':A1)^(':ARG2':A2)^s(A1,vp(v("be"),DSyntR,A2/pp(p("for"),A2)))):-
    memberchk(':ARG1',Keys).

%% HACK for imperative for which JSreal remove the subject
%%      do this only when :ARG0 [you,_] you appears...
checkSpecial(Roles,EnvIn,EnvOut,Options,[t("ip")|Options]):-
    hasRole(Roles,':mode','imperative',_),hasRole(Roles,':ARG0',['you',_],_),
    select(':&':q("let"),EnvIn,EnvOut),!.% remove q("let") added in processRole(':mode',..)
checkSpecial(Roles,Env,Env,Options,[typ({"int":"yon"})|Options]):- % special case of yes-or-no question
    hasRole(Roles,':ARG1',['amr-choice'|_],_).
checkSpecial(_,Env,Env,Options,Options). % keep as is

%% HACK for passive : only for verbs
%% check if conceptDSR has an :ARG0 and the actual roles does not have :ARG0 but has :ARG1
%% in that case, generate a passive sentence otherwise do nothing
checkPassive('bear-02',_,_,[]):-!. %% do not passivate the special case of bear-02 because it is already passive
checkPassive(Modal,_,_,[]):-modalityFlag(Modal,_,_,_). %% do not passivate modals
checkPassive(_,Roles,(':ARG0':_)^_,[typ({"pas":'true'})]):-
    \+hasRole(Roles,':ARG0',_,_),hasRole(Roles,':ARG1',_,_).
checkPassive(_,_,_,[]).

%% HACK for changing nominative pronoun to accusative for :ARG1 when :ARG0 is also present
checkAccusativePronoun(EnvIn,[':ARG1':AccPron|EnvOut]):-
    memberchk(':ARG0':_,EnvIn),
    select(':ARG1':NomPron,EnvIn,EnvOut),
    nominative2accusativePronoun(NomPron,AccPron).
checkAccusativePronoun(Env,Env).

%%% find nominalization if possible, fail otherwise
getNominalization([Concept,V],[NounVerb,V]):- % simple nominalization  (wihtout roles)
    verbalization(Concept,NounVerb),!.
getNominalization([Concept,V|Roles],AMR):- % complex verbalization
    verbalization(Concept,Role,Arg,NounVerb),
    hasRole(Roles,Role,[Arg|SubRole],RestRoles),
    matchSubRole([Arg|SubRole],Role,V,MatchedSubRoles),!,
    append([NounVerb,V|MatchedSubRoles],RestRoles,AMR).
getNominalization([Concept,V|Roles],[Nominalization,V|RolesOut]):-
    verb(Concept,_),getMorphVerb(Concept,Nominalization), %% concept is a verb with a nominalization 
    \+hasRole(Roles,':polarity',_,_),       %% fail if :polarity role is present
    (hasRole(Roles,':ARG1',Var,RolesOut )-> %% if :ARG1 is present
        \+hasArgOpRole(RolesOut),           %% fail if any :ARGi or :opi is present
        %% OK if :ARG1 is a pronoun that refers to the :ARG0 of the verb of the upper level
        atom(Var),getPaths(Var,VarPath,ConceptPath),
        ConceptPath=[_,':ARG0',Verb],append([':ARG1'],Path,VarPath),append(_P,[Verb],Path);
    \+hasArgOpRole(Roles), %% if no :ARG1 is present, fail if any :ARGi or :opi is present
    RolesOut=Roles
    ).
    
%%% complex verbalization ...
matchSubRole([_Verb,_VV|Roles],InvRole,V,RestRoles1):-
    re_replace('^:\\*(.*)$'/a,'\\1',InvRole,Role),
    hasRole(Roles,Role,V1,RestRoles),
    sameVar(V,V1),
    addMod(Role,RestRoles,RestRoles1).
 sameVar(V,V).
 sameVar(\V,V).
 sameVar(V,\V).
 addMod(_,[],[]).
 addMod(':ARG0',RestRoles,[[':poss',Val1]|RestRoles1]):-
     hasRole(RestRoles,':ARG1',Val1,RestRoles1).
 addMod(':ARG1',RestRoles,[[':poss',Val1]|RestRoles1]):-
     hasRole(RestRoles,':ARG0',Val1,RestRoles1).


%%% Environment management
applyEnv((Key:Var)^Expr0,Env0,Env2,Expr2):-
    select(Key:Val,Env0,Env1),
    reduce(Var^Expr0,Val,Expr1),
    applyEnv(Expr1,Env1,Env2,Expr2).
applyEnv((_Key:Var)^Expr0,Env0,Env1,Expr2):- % no value found, null given as value
    reduce(Var^Expr0,null,Expr1),
    applyEnv(Expr1,Env0,Env1,Expr2).
applyEnv(Expr,Env,Env,Expr).

getAllVals(Env,Vals):-
    findall(Val,member(_:Val,Env),Vals0),reverse(Vals0,Vals). % get back the original order... (HACK!)
getAllKeys(Env,Keys):-
    findall(Key,member(Key:_,Env),Keys0),
    delete(Keys0,':*',Keys1),% ignore the ':*' in the list of keys
    delete(Keys1,':&',Keys). % ignore the ':&' in the list of keys

showKeys([]):-!.
showKeys(X):-format('**unprocessed roles: ~w~n',X).

pprintEnv([]):-writeln("---").
pprintEnv([Key:Val|KVs]):-
    write(Key),write(':'),write_length(Key,L,[]),pprint(Val,L+1),
    pprintEnv(KVs).

%% add the values at the top level of S
addRestRoles(S*Options,Env,S1*Options):-!,addRestRoles(S,Env,S1).
addRestRoles(V^S,Env,V^S1):-!,addRestRoles(S,Env,S1).
addRestRoles(S,[],S):-!.
addRestRoles(S,Env,S1):-string(S),!,addRestRoles(s(S),Env,S1).
addRestRoles(S,Env,S1):-
     getPrefixVals(Env,PrefixVals,Env1),
     getPostfixVals(Env1,[],PostfixVals,Env2),
     getAllKeys(Env2,Keys),showKeys(Keys),
     getAllVals(Env2,RestVals),
    (S=..[q|Xs]      ->merge(PrefixVals,q,Xs,PostfixVals,RestVals,L),S1=..[ls|L];
     S=..[c|Ps]      ->merge(PrefixVals,c,Ps,PostfixVals,RestVals,L),S1=..[ls|L];
     S=..[a|Ps]      ->merge(PrefixVals,a,Ps,PostfixVals,RestVals,L),S1=..[ls|L];
     S=..[pro|Ps]    ->merge(PrefixVals,pro,Ps,PostfixVals,RestVals,L),S1=..[ls|L];
     S=..[np,D,A,[n|Ps]]->merge(PrefixVals,n,Ps,PostfixVals,RestVals,L),S1=..[np,D,A|L];
     (S=..[P|Xs], merge(PrefixVals,null,Xs,PostfixVals,RestVals,L),S1=..[P|L])).

getPrefixVals([],[],[]).
getPrefixVals([':&':Val|KVs],[Val|Vals1],Env):-!,getPrefixVals(KVs,Vals1,Env).
getPrefixVals([KV|KVs],Vals,[KV|Env])        :-  getPrefixVals(KVs,Vals,Env).

% as postFixVals have been added in front, they are reversed to follow original order in the AMR
getPostfixVals([],Vals,Vals,[]).
getPostfixVals([':*':Val|KVs],ValsIn,ValsOut,Env):-!,getPostfixVals(KVs,[Val|ValsIn],ValsOut,Env).
getPostfixVals([KV|KVs],ValsIn,ValsOut,[KV|Env])        :-  getPostfixVals(KVs,ValsIn,ValsOut,Env).

merge(Pre,null,Ps,Post,Rest,L2):-append(Pre,Ps,L0),append(L0,Post,L1),append(L1,Rest,L2).
merge(Pre,S,Ps,Post,Rest,L2):-F=..[S|Ps],append(Pre,[F],L0),append(L0,Post,L1),append(L1,Rest,L2).

%% add options
addOptions([],DSyntR,DSyntR).
addOptions([Option|Options],DSyntRin,DSyntROut*Option):-
    addOptions(Options,DSyntRin,DSyntROut).

%% create a predicate sentence DSyntR
predicate(Subject,s(null,VP)       ,s(Subject,VP)):-!.  % try to avoid adding embedded S
predicate(Subject,s(null,VP)*Option,s(Subject,VP)*Option):-!.
predicate(pro("whom"),Attribute,s(pro("who"),vp(v("be"),Attribute))):-!.
predicate(Subject,Attribute,s(Subject,vp(v("be"),Attribute))).

%% create a relative DSyntR
relative(Concept,InDSyntR,OutDSyntR):-
    gender(Concept,G),
    (G=="n"->Pronoun=pro("that");Pronoun=pro("who")),
    (isS(InDSyntR)->OutDSyntR=sp(Pronoun,InDSyntR);
                    predicate(Pronoun,InDSyntR,OutDSyntR)).
