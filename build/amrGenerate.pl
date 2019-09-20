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
:-['reification'].
:-['gophi'].

:-['inputPage'].
:-['replyPage'].

% %% get the value of a specific argument from a list returned by cgi_get_form,
% %%       get_arg(Arguments,NameOfParameter,Value)
% get_arg(Args,N,V):-NV=..[N,V],memberchk(NV,Args,_).

amrGenerate:-
    current_output(Stream),set_stream(Stream, encoding(utf8)),
    cgi_get_form(Arguments),
    get_arg(Arguments,amr,AMRstring),
    amrParseValidate(AMRstring,SemR,Errors),
    (Errors=""-> createStructs(SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors),
                 createHTML('amrVerbalizer.cgi',Arguments,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors);
      split_string(Errors,'\n','\n',ErrorList),
      inputPage('amrGenerate.cgi',Arguments,ErrorList)),
    halt(0).
