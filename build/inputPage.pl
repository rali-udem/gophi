:- encoding(utf8).

%% get the value of a specific argument from a list returned by cgi_get_form, 
%%       get_arg(Arguments,NameOfParameter,Value)
get_arg(Args,N,V):-NV=..[N,V],memberchk(NV,Args).


%% AMR shown at the start of the application
initialAMR('(d / desire-01
    :ARG0 (b/boy)
    :ARG1 (g/girl
           :ARG0-of (l/like-01
                       :polarity - 
                       :ARG1 b)))').

errors([])-->[].
errors([E|Es]) -->[E],html(br('')),errors(Es).

cbAttributes(Arguments,Name,[type=checkbox,name=Name,id=Name,checked]):-
    get_arg(Arguments,Name,on),!.
cbAttributes(_Arguments,Name,[type=checkbox,name=Name,id=Name]).

inputPage(Action,Arguments,ErrorList):-
   (get_arg(Arguments,amr,AMRstring);initialAMR(AMRstring)), %% get the AMR
   cbAttributes(Arguments,fol,FolAttrs),
   cbAttributes(Arguments,semR,SemRAttrs),
   cbAttributes(Arguments,dsyntR,DsyntRAttrs),
   cbAttributes(Arguments,ssyntR,SsyntRAttrs),
   reply_html_page(
       [title('GoPhi: an AMR Verbalizer'),
        script(src='http://code.jquery.com/jquery-latest.min.js',''),
        script(src='gophi-web/ace/ace.js',''),
        script(src='gophi-web/amr-verb.js',''),
        link([href='gophi-web/amr-verb.css', type='text/css',rel='stylesheet'])
       ],
       [form(
        [action=Action, method='POST',id='form'], 
        [h1([a([href='https://github.com/rali-udem/gophi'],['Γω-Φ']),': an AMR verbalizer']),
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
             label([for=fol],'First-Order Logic'),input(FolAttrs,[]),&(nbsp),
             label([for=semR],'Semantic'),input(SemRAttrs,[]),&(nbsp),
             label([for=dsyntR],'Deep Syntactic'),input(DsyntRAttrs,[]),&(nbsp),
             label([for=ssyntR],'Surface Syntactic'),input(SsyntRAttrs,[])
            ]
        )
        ])
   ]).

gotoFirstError([]) --> [].
gotoFirstError([Error|_]) -->
    {re_matchsub("^Line (\\d+):(\\d+) :",Error,Sub,[])} ->
        html(script('$(document).ready(function() {ace.edit("amr").gotoLine(~w,~w,true);})'-[Sub.1,Sub.2]));
        html(script('$(document).ready(function() {ace.edit("amr").navigateFileEnd();})')).
