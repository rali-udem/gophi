#!/usr/bin/env swipl -f -q
:- initialization amrVerbalizer.
% can be executed as is on the command line
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

initialAMR('(d / desire-01
    :ARG0 (b/boy)
    :ARG1 (g/girl
           :ARG0-of (l/like-01
                       :polarity - 
                       :ARG1 b)))').

amrVerbalizer :-
    current_output(Stream),set_stream(Stream, encoding(utf8)),
    cgi_get_form(Arguments),
    (get_arg(Arguments,amr,AMRString);
     initialAMR(AMRString)),
    inputPage(AMRString,[]),
    halt(0).
