:- encoding(utf8).

:- dynamic currentAMR/2.

:-['utils'].
:-['checkParse'].
:-['dictionary'].
:-['deepSyntR'].
:-['surfSyntR'].
:-['semantics'].
:-['gophi'].

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

%% get the value of a specific argument from a list returned by http_read_data, 
%%       get_arg(Arguments,NameOfParameter,Value)
get_arg(Data,N,V):-memberchk(N=V,Data).

:- ['inputPage'].
:- ['replyPage'].

% for file serving
:- use_module(library(http/http_files)).
http:location(files, '/gophi-web', []).
:- http_handler(files(.), http_reply_from_files('./gophi-web', []), [prefix]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler('/amrVerbalizer', amrVerbalizer, []).
amrVerbalizer(Request) :-
    (member(method(post), Request)->
        http_read_data(Request, Data, [content_type('application/x-prolog')]),
        memberchk(amr=AMRstring,Data);
    initialAMR(AMRstring)),
    inputPage('amrGenerate',AMRstring,[]).

:- http_handler('/amrGenerate', amrGenerate, []).
amrGenerate(Request) :-
    http_read_data(Request, Data, [content_type('application/x-prolog')]),
    (Data=end_of_file->true;
     memberchk(amr=AMRstring,Data),
     amrParseValidate(AMRstring,SemR,Errors),
     (Errors=""-> createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors),
                  createHTML('amrVerbalizer',Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors);
      split_string(Errors,'\n','\n',ErrorList),
      inputPage('amrGenerate',AMRstring,ErrorList)
     )
    ).

%% restart server when reloading
:-catch(http_stop_server(8000,[]),_,true),
    server(8000),writeln("Browse: http://127.0.0.1:8000/amrVerbalizer").