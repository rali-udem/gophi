:- encoding(utf8).

%% process a single AMR 

% transforms an AMR to a SSyntR
%    AMRstring : AMR to transform
%    SSyntR : resulting SSyntR
%    ShowStructs : if true pretty prints intermediary structures
%    TraceTrans : if true starts tracing after parsing the AMR (type l(eap) to continue without tracing)
amr2SSyntR(AMRstring,SSyntR,ShowStructs,TraceTrans):-
    amrParse(AMRstring,AMR),
    ignore(ShowStructs->(write('> Semantic Representation\n'),pprint(AMR))),!,
    dereify(AMR,AMR1),
    ignore((ShowStructs,AMR \= AMR1)->( % ne pas afficher si la dereification ne change rien
        write('> Semantic Representation dereified\n'),pprint(AMR1),nl)),     
    elimInv(AMR1,AMRnoInv),
    getVarRefs(AMRnoInv,[],Vars),processVars(AMRnoInv,Vars,AMRout,VarsOut),
    ignore((ShowStructs,AMR1 \= AMRout)->( % ne pas afficher si pas de role inverse ou de références répétées
        write('> Semantic Representation without Inverse Roles\n'),pprint(AMRout),nl)), 
    % save the current parsed AMR and its list of variables 
    % used for pronoun generation by getRef/4 in pronounReference.pl
    retractall(currentAMR(_,_)),assert(currentAMR(AMRout,VarsOut)), 
    ignore(TraceTrans->trace), % enable trace for the rest of amr2SSyntR
    amr2dsr(AMRout,_Concept,_POS,DSyntR),
    ignore(ShowStructs->(write('> Deep Syntactic Representation\n'),pprint(DSyntR))),!,
    phrase(dsr2jsReal(DSyntR),Tokens),!,
    atomic_list_concat(Tokens,'',SSyntR),
    ignore(ShowStructs->(write('> Surface Syntactic Representation\n'),write(SSyntR),nl)).

processVars(AMRin,Vars,AMRout,VarsOut):-
    hasRepetition(Vars),  % do not try to separate references if there no repeeated vars
    separateRefInstances(AMRin,[],AMRout,VarsOut).
processVars(AMR,Vars,AMR,Vars).
    

% amr2SSyntR(AMRstring,SSyntR):-amr2SSyntR(AMRstring,SSyntR,false,false).
amr2BaseGen(AMRstring,BaseGen):-
    amrParse(AMRstring,AMR),baseGen(AMR,Words,[]),atomic_list_concat(Words,' ',BaseGen).    

% call the jsRealServer to write the realization
% server should previously be launched with
%  node /Users/lapalme/Documents/GitHub/jsRealB/build/server-dme.js
:- use_module(library(http/http_open)).

jsRealBserver(SSyntR,Sent):-
    uri_encoded(query_value, SSyntR, StrEncoded),
    atom_concat('http://127.0.0.1:8081/?lang=en&exp=', StrEncoded, URL),
    http_open(URL, In, []),
    set_stream(In,encoding(utf8)),
    read_string(In, "\n", "\r", _, Sent),
    close(In).

% call the jsReal filter as a subprocess 
% NB: it is created at each call because I did not find how to keep the subprocess going and 
%     sending it multiple input after closing (I will keep trying)
%     this seems much slower than the server version, and we do not have a trace of a JS error... 
%     but it does not require launching the server process
jsRealBfilter(NodePath,FilterPath,SSyntR,Sent):-
    process_create(NodePath,
            [FilterPath],
            [stdin(pipe(In)),stdout(pipe(Out))]),
    re_replace("\n"/ga,"",SSyntR,Input), % remove ends of line because the filter only reads a single line
    set_stream(In,encoding(utf8)),    
    write(In,Input),close(In),
    set_stream(Out,encoding(utf8)),
    read_string(Out, "\n", "\r", _, Sent),close(Out).

%% select the type of interaction by (un)commenting the appropriate 
jsRealB(SSyntR,Sent):-jsRealBserver(SSyntR,Sent).
% jsRealB(SSyntR,Sent):-
%    jsRealBfilter('/usr/local/bin/node','/Users/lapalme/Documents/GitHub/jsRealB/build/filter-dme.js',SSyntR,Sent).

%% full process calling the realisation
gophi(AMRString,Print,Trace):-
    amr2SSyntR(AMRString,SSyntR,Print,Trace),
    jsRealB(SSyntR,GenSent),
    write('> English sentence\n'),
    writeln(GenSent),!.

testAMRstring(AMRString):- gophi(AMRString,true,false).
traceAMRstring(AMRString):-gophi(AMRString,false,true).

%%% simple test of examples
testEx(Id):-
        ex(Id,S,Sent),format('~s~s~s~s~n',['--- ',Id,':',Sent]),
        testAMRstring(S).

simpleExamples(['1a','1b','1c','2','2b','3','4a','4b','4c','5a','5b','9a']).
testExs:-forall((simpleExamples(Exs),member(Ex,Exs)),testEx(Ex)).
testOfs:-forall((exOf(Exs),member(Ex,Exs)),testEx(Ex)).
