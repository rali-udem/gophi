:- encoding(utf8).

%%%% create content of web page
createStructs(SemR,FOL,SemRout,DSyntR,SSyntR,SemRerrors):-
    combineSem(SemR,false,FOL),
    elimInv(SemR,SemRnoInv),
    getVarRefs(SemRnoInv,[],Vars),processVars(SemRnoInv,Vars,SemRout,VarsOut),
    % save the current parsed SemR and its list of variables 
    % used for pronoun generation by getRef/4 in pronounReference.pl
    retractall(currentAMR(_,_)),assert(currentAMR(SemRout,VarsOut)), 
    with_output_to(string(SemRerrors),
        amr2dsr(SemRout,_Concept,_POS,DSyntR)),
    phrase(dsr2jsReal(DSyntR),Tokens),!,
    atomic_list_concat(Tokens,'',SSyntR).

createHTML(Action,Data,AMRstring,SemR,FOL,SemRnoInv,DSyntR,SSyntR,SemRerrors):-
        reply_html_page(
            [title('AMR verbalized'),
             script([src='gophi-web/jsRealB-dme.min.js'],''),
             script([src='gophi-web/addLexicon-dme.js'],''),
             style('.sent {font-weight:bold} textarea {font-family:monospace;font-size:large}')],
            [h1(['AMR verbalized by ',a([href='https://github.com/rali-udem/gophi'],['Γω-Φ'])]),
             h2('AMR'),
             form([action=Action,method='POST',id='form'],
             [pre(code([AMRstring])),
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
              input([name=amr,type=hidden,value=AMRstring],[]), % send back the AMR for editing
              input([name=submit, type=submit, value='Edit the AMR'], [])
             ])
            ]
        ).


showFOL(Data,FOL)-->{get_arg(Data,fol,on)},!,
    html([h2('First-Order Logic'),pre(\ppSem(FOL))]).
showFOL(_,_)-->[].

showSemR(Data,SemR,SemRnoInv)-->{get_arg(Data,semR,on)},!,
    html([h2('Semantic Representation'),pre(\pp(SemR)),\showSemRinv(SemR,SemRnoInv)]).
showSemR(_,_,_)-->[].

showSemRinv(SemR,SemRnoInv) --> {SemR \= SemRnoInv},!,
     html([h2('Semantic Representation without inverse roles'),pre(\pp(SemRnoInv))]).
showSemRinv(_,_)-->[].

showDSyntR(Data,DSyntR)-->{get_arg(Data,dsyntR,on)},!,
    html([h2('Deep Syntactic Representation'),pre(\pp(DSyntR))]).
showDSyntR(_,_)-->[].

showSSyntR(Data,SSyntR)-->{get_arg(Data,ssyntR,on)},!,
    html([h2('Surface Syntactic Representation'),pre(SSyntR)]).
showSSyntR(_,_)-->[].

showSemRerrors("")-->[].
showSemRerrors(Errors)-->
    {split_string(Errors,'\n','\n',ErrorList)},
    html([h3('Errors found in creating the Semantic Representation'),
          p(\errors(ErrorList))]).
