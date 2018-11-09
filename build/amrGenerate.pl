#!/usr/bin/env swipl -f -q
:- initialization amrGenerate.
% can be executed as a command line script (but it will call a cgi script afterward)
% but to compile, remove the first two lines of the script 
%    tail -n +3 amrGenerate.pl > amrGenerate2.pl
%    swipl -o amrGenerate.cgi -g amrGenerate -t halt -c amrGenerate2.pl
% 
% move amrGenerate.cgi to a directory in which cgis can be accessed
% move also directory "gophiWeb" to the same directory

:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(cgi)).

:- encoding(utf8).

:- dynamic currentAMR/2.

:-['utils'].
:-['checkParse'].
:-['dictionary'].
:-['deepSyntR'].
:-['surfSyntR'].
:-['semantics'].
:-['gophi'].

:-['inputPage'].
:-['replyPage'].

%% get the value of a specific argument from a list returned by cgi_get_form, 
%%       get_arg(Arguments,NameOfParameter,Value)
get_arg(Args,N,V):-NV=..[N,V],selectchk(NV,Args,_).

amrGenerate:-
    current_output(Stream),set_stream(Stream, encoding(utf8)),
    cgi_get_form(Arguments),
    get_arg(Arguments,amr,AMRstring),
    amrParseValidate(AMRstring,SemR,Errors),
    (Errors=""-> createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors),
                 createHTML('amrVerbalizer.cgi',Arguments,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors);
      split_string(Errors,'\n','\n',ErrorList),
      inputPage('amrGenerate.cgi',AMRstring,ErrorList)),
    halt(0).

% amrGenerate(Request) :-
%     http_read_data(Request, Data, [content_type('application/x-prolog')]),
%     (Data=end_of_file->true;
%      memberchk(amr=AMRstring,Data),
%      amrParseValidate(AMRstring,SemR,Errors),
%      (Errors=""-> createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors),
%                   createHTML(Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors);
%       split_string(Errors,'\n','\n',ErrorList),inputPage(AMRstring,ErrorList))).

% %%%% create content of web page
% createStructs(SemR,FOL,SemRout,DSyntR,SSyntR,SemRerrors):-
%     combineSem(SemR,false,FOL),
%     elimInv(SemR,SemRnoInv),
%     getVarRefs(SemRnoInv,[],Vars),processVars(SemRnoInv,Vars,SemRout,VarsOut),
%     % save the current parsed SemR and its list of variables
%     % used for pronoun generation by getRef/4 in deepSyntR.pl
%     retractall(currentAMR(_,_)),assert(currentAMR(SemRout,VarsOut)),
%     with_output_to(string(SemRerrors),
%         amr2dsr(SemRout,_Concept,_POS,DSyntR)),
%     phrase(dsr2jsReal(DSyntR),Tokens),!,
%     atomic_list_concat(Tokens,'',SSyntR).
%
% countLines(Atom,N):-
%     split_string(Atom,'\n','\n',Ss),length(Ss,N).
%
% createHTML(Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors):-
%         countLines(AMRstring,NbLines),
%         reply_html_page(
%             [title('AMR verbalized'),
%              script([src='gophi-web/jsRealB-dme.min.js'],''),
%              script([src='gophi-web/addLexicon-dme.js'],''),
%              style('.sent {font-weight:bold} textarea {font-family:monospace;font-size:large}')],
%             [h1('AMR verbalized by Γω-Φ'),
%              h2('AMR'),
%              form([action='amrVerbalizer.cgi',method='POST',id='form'],
%              [textarea([name=amr,cols=80,rows=NbLines,readonly=readonly],AMRstring),
%               \showFOL(Data,FOL),
%               \showSemR(Data,SemR,SemRnoInv),
%               \showDSyntR(Data,DSyntR),
%               \showSSyntR(Data,SSyntR),
%               \showSemRerrors(SemRerrors),
%               h2('English sentence'),
%               p([class=sent],
%                 \js_script({|javascript(SSyntR)||
%                     loadEn();
%                     sent=eval(SSyntR).toString();            // generate sentence
%                     sent=sent.replace(/\[\[(.*?)\]\]/g,'$1') // clean unknown word flags
%                     document.write(sent);
%               |})),
%               input([name=submit, type=submit, value='Edit the AMR'], [])
%              ])
%             ]
%         ).
%
%
% showFOL(Data,FOL)-->{get_arg(Data,fol,on)},!,
%     html([h2('First-Order Logic'),pre(\ppSem(FOL))]).
% showFOL(_,_)-->[].
%
% showSemR(Data,SemR,SemRnoInv)-->{get_arg(Data,semR,on)},!,
%     html([h2('Semantic Representation'),pre(\pp(SemR)),\showSemRinv(SemR,SemRnoInv)]).
% showSemR(_,_,_)-->[].
%
% showSemRinv(SemR,SemRnoInv) --> {SemR \= SemRnoInv},!,
%      html([h2('Semantic Representation without inverse roles'),pre(\pp(SemRnoInv))]).
% showSemRinv(_,_)-->[].
%
% showDSyntR(Data,DSyntR)-->{get_arg(Data,dsyntR,on)},!,
%     html([h2('Deep Syntactic Representation'),pre(\pp(DSyntR))]).
% showDSyntR(_,_)-->[].
%
% showSSyntR(Data,SSyntR)-->{get_arg(Data,ssyntR,on)},!,
%     html([h2('Surface Syntactic Representation'),pre(SSyntR)]).
% showSSyntR(_,_)-->[].
%
% showSemRerrors("")-->[].
% showSemRerrors(Errors)-->
%     {split_string(Errors,'\n','\n',ErrorList)},
%     html([h3('Errors found in creating the Semantic Representation'),
%           p(\errors(ErrorList))]).
