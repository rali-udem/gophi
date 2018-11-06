:- encoding(utf8).

%% set-up the AMR Verbalizer web server
%%    Web page is built using "Termerized HTML" or HTML//1

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)). %% comment this line when in "production"
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/js_write)).

% for file serving
:- use_module(library(http/http_files)).
http:location(files, '/gophi-web', []).
:- http_handler(files(.), http_reply_from_files('./gophi-web', []), [prefix]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

/* 
   once server(8000). has been called, browse http://127.0.0.1:8000/AMR-Verbalizer 
   the server can be stopped by calling http_stop_server(8000,[]).
*/

initialAMR('(d / desire-01
    :ARG0 (b/boy)
    :ARG1 (g/girl
           :ARG0-of (l/like-01
                       :polarity - 
                       :ARG1 b)))').

:- http_handler('/AMR-Verbalizer', amrVerbalizer, []).

amrVerbalizer(Request) :-
    (member(method(post), Request)->
        http_read_data(Request, Data, [content_type('application/x-prolog')]),
        memberchk(amr=AMRstring,Data);
    initialAMR(AMRstring)),
    inputPage(AMRstring,[]).

errors([])-->[].
errors([E|Es]) -->[E],html(br('')),errors(Es).

inputPage(AMRstring,ErrorList):-
   reply_html_page(
       [title('AMR Verbalizer'),
        script(src='http://code.jquery.com/jquery-latest.min.js',''),
        script(src='gophi-web/ace/ace.js',''),
        script(src='gophi-web/amr-verb.js',''),
        link([href='gophi-web/amr-verb.css', type='text/css',rel='stylesheet'])
       ],
       [form(
        [action='/amr-generate', method='POST',id='form'], 
        [h1(['Γω-Φ: an AMR verbalizer']),
         p(['AMR Color coding: ',
            span(class='ace_variable','variable'),', ',
            span(class='ace_concept','concept'),', ',
            span(class='ace_role','role')
         ]),
         div(id='amr',''),
         div(id='inputAMR',AMRstring),
         textarea([name=amr],''),
         input([name=submit, type=submit, value='Verbalize'], []),
         p(\errors(ErrorList)),
         \gotoFirstError(ErrorList),
         fieldset(
            [legend('Show Representations'),
             label([for=fol],'First-Order Logic'),input([type=checkbox,name=fol,id=fol],[]),&(nbsp),
             label([for=semR],'Semantic'),input([type=checkbox,name=semR,id=semR],[]),&(nbsp),
             label([for=dsyntR],'Deep Syntactic'),input([type=checkbox,name=dsyntR,id=dsyntR],[]),&(nbsp),
             label([for=ssyntR],'Surface Syntactic'),input([type=checkbox,name=ssyntR,id=ssyntR],[])
            ]
        )
        ])
   ]).

gotoFirstError([]) --> [].
gotoFirstError([Error|_]) -->
    {re_matchsub("^Line (\\d+):(\\d+) :",Error,Sub,[])} ->
        html(script('$(document).ready(function() {ace.edit("amr").gotoLine(~w,~w,true);})'-[Sub.1,Sub.2]));
        html(script('$(document).ready(function() {ace.edit("amr").navigateFileEnd();})')).

:- dynamic currentAMR/2.

:-['utils'].
:-['checkParse'].
:-['dictionary'].
:-['deepSyntR'].
:-['surfSyntR'].
:-['semantics'].
:-['gophi'].

:- http_handler('/amr-generate', amrGenerate, []).

amrGenerate(Request) :-
    http_read_data(Request, Data, [content_type('application/x-prolog')]),
    (Data=end_of_file->true;
     memberchk(amr=AMRstring,Data),
     amrParseValidate(AMRstring,SemR,Errors),
     (Errors=""-> createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors),
                  createHTML(Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors);
      split_string(Errors,'\n','\n',ErrorList),inputPage(AMRstring,ErrorList))).

%%%% create content of web page
createStructs(SemR,FOL,SemRout,DSyntR,SSyntR,SemRerrors):-
    combineSem(SemR,false,FOL),
    elimInv(SemR,SemRnoInv),
    getVarRefs(SemRnoInv,[],Vars),processVars(SemRnoInv,Vars,SemRout,VarsOut),
    % save the current parsed SemR and its list of variables 
    % used for pronoun generation by getRef/4 in deepSyntR.pl
    retractall(currentAMR(_,_)),assert(currentAMR(SemRout,VarsOut)), 
    with_output_to(string(SemRerrors),
        amr2dsr(SemRout,_Concept,_POS,DSyntR)),
    phrase(dsr2jsReal(DSyntR),Tokens),!,
    atomic_list_concat(Tokens,'',SSyntR).

:- http_handler('/AMR-verb/verbalize', amrToEnglish, []).
amrToEnglish(Request) :-
        http_read_data(Request, Data, [content_type('application/x-prolog')]),
        memberchk(amr=AMRstring,Data),
        createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,Errors),
        createHTML(Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,Errors).

countLines(Atom,N):-
    split_string(Atom,'\n','\n',Ss),length(Ss,N).

createHTML(Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors):-
        countLines(AMRstring,NbLines),
        reply_html_page(
            [title('AMR verbalized'),
             script([src='gophi-web/jsRealB-dme.min.js'],''),
             script([src='gophi-web/addLexicon-dme.js'],''),
             style('.sent {font-weight:bold} textarea {font-family:monospace;font-size:large}')],
            [h1('AMR verbalized by Γω-Φ'),
             h2('AMR'),
             form([action='AMR-Verbalizer',method='POST',id='form'],
             [textarea([name=amr,cols=80,rows=NbLines,readonly=readonly],AMRstring),
              \showFOL(Data,FOL),
              \showSemR(Data,SemR,SemRnoInv),
              \showDSyntR(Data,DSyntR),
              \showSSyntR(Data,SSyntR),
              \showSemRerrors(SemRerrors),
              h2('English sentence'),
              p([class=sent],
                \js_script({|javascript(SSyntR)||
                    loadEn();
                    sent=eval(SSyntR).toString();            // generate sentence
                    sent=sent.replace(/\[\[(.*?)\]\]/g,'$1') // clean unknown word flags
                    document.write(sent); 
              |})),
              input([name=submit, type=submit, value='Edit the AMR'], [])
             ])
            ]
        ).


showFOL(Data,FOL)-->{memberchk(fol=on,Data)},!,
    html([h2('First-Order Logic'),pre(\ppSem(FOL))]).
showFOL(_,_)-->[].

showSemR(Data,SemR,SemRnoInv)-->{memberchk(semR=on,Data)},!,
    html([h2('Semantic Representation'),pre(\pp(SemR)),\showSemRinv(SemR,SemRnoInv)]).
showSemR(_,_,_)-->[].

showSemRinv(SemR,SemRnoInv) --> {SemR \= SemRnoInv},!,
     html([h2('Semantic Representation without inverse roles'),pre(\pp(SemRnoInv))]).
showSemRinv(_,_)-->[].

showDSyntR(Data,DSyntR)-->{memberchk(dsyntR=on,Data)},!,
    html([h2('Deep Syntactic Representation'),pre(\pp(DSyntR))]).
showDSyntR(_,_)-->[].

showSSyntR(Data,SSyntR)-->{memberchk(ssyntR=on,Data)},!,
    html([h2('Surface Syntactic Representation'),pre(SSyntR)]).
showSSyntR(_,_)-->[].

showSemRerrors("")-->[].
showSemRerrors(Errors)-->
    {split_string(Errors,'\n','\n',ErrorList)},
    html([h3('Errors found in creating the Semantic Representation'),
          p(\errors(ErrorList))]).

%% restart server when reloading
:-catch(http_stop_server(8000,[]),_,true),
    server(8000),writeln("Browse: http://127.0.0.1:8000/AMR-Verbalizer").