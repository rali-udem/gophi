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

amrVerbalizer :-
    current_output(Stream),set_stream(Stream, encoding(utf8)),
    cgi_get_form(Arguments),
    inputPage('amrGenerate.cgi',Arguments,[]),
    halt(0).
