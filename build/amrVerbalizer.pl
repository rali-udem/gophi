#!/usr/bin/env swipl -f -q
:- initialization amrVerbalizer.
% can be executed as a command line script (but it will call a cgi script afterward)
% but to compile, remove the first two lines of the script 
%    tail -n +3 amrVerbalizer.pl > amrVerbalizer2.pl
%    swipl -o amrVerbalizer.cgi -g amrVerbalizer -t halt -c amrVerbalizer2.pl
% 
% move amrVerbalizer.cgi to a directory in which cgis can be accessed

:- encoding(utf8).
:- use_module(library(http/html_write)). 
:- use_module(library(http/js_write)).
:- use_module(library(cgi)).
:- [inputPage].

%% get the value of a specific argument from a list returned by cgi_get_form, 
%%       get_arg(Arguments,NameOfParameter,Value)
get_arg(Args,N,V):-NV=..[N,V],selectchk(NV,Args,_).

amrVerbalizer :-
    current_output(Stream),set_stream(Stream, encoding(utf8)),
    cgi_get_form(Arguments),
    (get_arg(Arguments,amr,AMRString);
     initialAMR(AMRString)),
     inputPage('amrGenerate.cgi',AMRString,[]),
    halt(0).
