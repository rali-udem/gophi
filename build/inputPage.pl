:- encoding(utf8).
%% get the value of a specific argument from a list returned by cgi_get_form, 
%%       get_arg(Arguments,NameOfParameter,Value)
get_arg(Args,N,V):-NV=..[N,V],selectchk(NV,Args,_).

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
        [action='amrGenerate.cgi', method='POST',id='form'], 
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
