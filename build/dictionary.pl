:- encoding(utf8).
:- dynamic noun/2.
:- dynamic adjective/2.
:- dynamic verb/2.
:- dynamic adverb/2.
:- dynamic preposition/2.
:- dynamic verbalization/2.
:- dynamic verbalization/4.
:- dynamic pronoun/2.
:- dynamic conjunction/2.
:- dynamic determiner/2.

:- ['dictionaryGenerated']. % load generated dictionary
:- ['gender'].
:- ['dictionary-utils'].

% ====== Pronouns
%  addition to the dictionary
:-patch('pronoun','I',pro("I")*pe(1)*g("m")*n("s")).
pronoun('i',pro("I")*pe(1)*g("m")*n("s")). % i is often used in AMR examples
pronoun('you',pro("I")*pe(2)*g("m")*n("s")).
pronoun('he',pro("I")*pe(3)*g("m")*n("s")).
pronoun('she',pro("I")*pe(3)*g("f")*n("s")).
pronoun('it',pro("I")*pe(3)*g("n")*n("s")).
pronoun('we',pro("I")*pe(1)*g("m")*n("p")).
% ambiguity on the plural you
pronoun('they',pro("I")*pe(3)*g("m")*n("p")).

% hack here: used to get the appropriate possessive when returned by getRef
pronoun('I',pro("me")*pe(1)*g(_G)*n(_N)).
pronoun('I',pro("myself")*pe(1)*g(_G)*n(_N)).

% pronoun('everything',pro("everything")).
% pronoun('something',pro("something")).
% pronoun('anything',pro("anything")).
% pronoun('nothing',pro("nothing")).
% pronoun('everyone',pro("everyone")).
% pronoun('someone',pro("someone")).
% pronoun('anyone',pro("anyone")).
% pronoun('no-one',pro("no one")).
pronoun('one',pro("one")).

possessive('I',d("my")*pe(1)*g("m")*n("s")).
possessive('i',d("my")*pe(1)*g("m")*n("s")). 
possessive('you',d("my")*pe(2)*g("m")*n("s")).
possessive('he',d("my")*pe(3)*g("m")*n("s")).
possessive('she',d("my")*pe(3)*g("f")*n("s")).
possessive('it',d("my")*pe(3)*g("n")*n("s")).
possessive('we',d("my")*pe(1)*g("m")*n("p")).
possessive('they',d("my")*pe(3)*g("m")*n("p")).

% ====== Determiners
% determiner('that',d("that")).
% determiner('a',d("a")).
determiner('this',d("this")).
determiner('another',d("another")).
determiner('every',d("every")).

% ====== Conjunctions
op16(C,(':op1':O1)^(':op2':O2)^(':op3':O3)^(':op4':O4)^(':op5':O5)^(':op6':O6)
        ^cp(c(C),O1,O2,O3,O4,O5,O6)).

:-delete('conjunction','before').
:-delete('conjunction','after'). % keep only prep

:-op16("and",Conj),patch('conjunction','and',Conj).
:-op16("or",Conj),patch('conjunction','or',Conj).
:-op16("either",Conj),patch('conjunction','either',Conj).
conjunction('slash',Conj):-op16("/",Conj).
% HACK here for arithmetical operators
conjunction('sum-of',Arith)    :-op16("and",Arith).
conjunction('product-of',Arith):-op16("times",Arith).
conjunction('ratio-of',(':op1':O1)^(':op2':O2)^np(n("ratio"),p("of"),O1,O2/pp(p("to"),O2))).
conjunction('difference-of',Arith):-op16("minus",Arith).
conjunction('quotient-of',Arith):-op16("divided by",Arith).
conjunction('power-of',Arith):-op16("to the power of",Arith).
conjunction('root-of',(':op1':O1)^(':op2':O2)^np(no(O2)*ord(true),n("root"),O1/pp(p("of"),O1))).
conjunction('logarithm-of',(':op1':O1)^(':op2':O2)^np(n("logarithm"),p("of"),O1,O2/ls(q("base:"),O2))).

% ====== Adverbs
adverb('there',adv("there")).
adverb('et-cetera',q("etc")).
adverb('at-least',ls(p("at"),adv("least"))).
adverb('next-to',(':op1':O1)^advp(adv("next"),p("to"),O1)).
:-delete('adverb','between').
:-delete('adverb','near').
:-delete('adverb','this').
:-delete('adverb','that').
:-delete('adverb','after'). % keep only prep

% ====== Prepositions
:-patch('preposition','after',(':op1':X)^pp(p("after"),X)).
:-patch('preposition','near',(':op1':X)^pp(p("near"),X)).
:-patch('preposition','up-to',(':op1':X)^pp(p("up"),p("to"),X)).
:-patch('preposition','between',(':op1':X)^(':op2':Y)^pp(p("between"),X,c("and"),Y)).

% ====== Verbs
:-delete('verb','contrast-01'). % add as noun
:-delete('verb','infer-01').

:-patch('verb','exemplify-01',ls(p("for"),n("example"))).

% :-patch('verb','contrast-01',(':ARG1':A1)^(':ARG2':A2)^s(A1,"but",A2)).
% :-patch('verb','infer-01',(':ARG1':A1)^(':ARG2':A2)^s(A1,"so",A2)).
:-patch('verb','give-01',(':ARG0':A)^(':ARG1':B)^(':ARG2':C)^
                         s(A,vp(v("give"),B,C/pp(p("to"),C)))).
:-patch('verb','know-01',(':ARG0':X0)^(':ARG1':X1)^(':ARG2':X2)^s(X0,vp(v("know"),X1,X2))). % was v("idk")!
%% in PropBank : bear-02 :ARG0 should be the "bearer" (e.g. the mother) and :ARG1 the "bearee" 
%% but in AMR, :ARG1 is used as the person who is born... without :ARG0
:-patch('verb','bear-02',(':ARG0':X0)^(':ARG1':X1)^s(X1,vp(v("be"),v("born")*t("pp"),X0/pp(p("to"),X0)))).
%% in PropBank: hunger-01 :ARG0 corresponds to the person who is hungry (hunger is archaic in this acception)
%% so we translate with a more colloquial "be hungry [for...]"
:-patch('verb','hunger-01',(':ARG0':X0)^(':ARG1':X1)^s(X0,vp(v("be"),a("hungry")*t("pp"),X1/pp(p("for"),X1)))).

:-delete('verb','war-01').
:-delete('verb','policy-01').
:-delete('verb','slew').

%% modality
:-patch('verb','possible-01',(':ARG1':A)^
                s(pro("I")*pe(3)*g("n"),vp(v("be"),a("possible"),A))).
:-patch('verb','obligate-01',(':ARG2':A)^
                s(pro("I")*pe(3)*g("n"),vp(v("be"),a("obligatory"),A))).
:-patch('verb','permit-01',(':ARG1':A)^
                s(pro("I")*pe(3)*g("n"),vp(v("be"),a("allowed"),A))).
:-patch('verb','recommend-01',(':ARG1':A)^
                s(pro("I")*pe(3)*g("n"),vp(v("be"),a("recommended")*t("pp"),A))).
:-patch('verb','likely-01',(':ARG1':A)^
                s(pro("I")*pe(3)*g("n"),vp(v("be"),a("likely"),A))).
% reifications 
verb('be-destined-for-91',(':ARG1':A1)^(':ARG2':A2)
                         ^s(A1,vp(v("be"),A2/pp(p("for"),A2)))).
verb('be-from-91',(':ARG1':A1)^(':ARG2':A2)
                         ^s(A1,vp(v("be"),A2/pp(p("from"),A2)))).
verb('be-with-10',(':ARG0':A0)^(':ARG1':A1)
                         ^s(A0,vp(v("be"),A1/pp(p("with"),A1)))).
verb('be-temporally-at-91',(':ARG1':A1)^(':ARG2':A2)
                         ^s(A1,vp(v("be"),A2/pp(p("on"),A2)))).
verb('be-located-at-91',(':ARG1':A1)^(':ARG2':A2)^(':time':T)
                         ^s(A1,vp(v("be"),A2/pp(p("in"),A2),T))).
verb('have-concession-91',(':ARG1':A1)^(':ARG2':A2)
                         ^s(A1,vp(v("be"),A2/pp(p("despite"),A2)))).
verb('have-condition-91',(':ARG1':A1)^(':ARG2':A2)
                         ^s(A1,vp(v("be"),A2/cp(c("if"),A2)))).

% ====== Adjectives
:-delete('adjective','near'). % keep only the preposition
:-delete('adjective','after'). % keep only prep

% ====== Nouns
% keep only the adjective
:-delete('noun','good').  
:-delete('noun','bad').
:-delete('noun','large').
:-delete('noun','small').
:-delete('noun','common').
:-delete('noun','no').
:-delete('noun','extreme').
%% keep only the verb
:-delete('noun','like').
% redefined later
:-delete('noun','score-entity').
:-delete('noun','contrast-01').

:-patch('noun','name',(':op1':O1)^(':op2':O2)^(':op3':O3)^(':op4':O4)^(':op5':O5)^(':op6':O6)^(':op7':O7)^
                        ls(O1+O2+O3+O4+O5+O6+O7)).
% add words to the dictionary
newNouns(['constrictor','media','tsunami']).
:- forall((newNouns(Ns),member(W,Ns)),
          (atom_string(W,WS),patch('noun',W,('D':D)^('A':A)^np($D/d("the"),A,n(WS))))).
:- forall(member(W,['sunday','monday','tuesday','wednesday','thursday','friday','saturday']),
           (capitalize(W,WS),patch('noun',W,('A':A)^np(A,n(WS))))).

:- patch('noun','government-organization',
                ('D':D)^np($D/d("the"),a("governmental"),n("organization"))).
:- patch('noun','criminal-organization',
                ('D':D)^np($D/d("the"),a("criminal"),n("organization"))).
:- patch('noun','religious-group',
                ('D':D)^np($D/d("the"),a("religious"),n("group"))).
:- patch('noun','ethnic-group',
                ('D':D)^np($D/d("the"),a("ethnic"),n("group"))).
:- patch('noun','political-movement',
                ('D':D)^np($D/d("the"),a("political"),n("movement"))).
:- patch('noun','political-party',
                ('D':D)^np($D/d("the"),a("political"),n("party"))).
:- patch('noun','world-region',
                ('D':D)^np($D/d("the"),n("world"),n("region"))).
% byline-91 frame arguments
% :ARG0 news organization :ARG1 author :ARG2 photographer, illustrator :ARG3 translator :ARG4 means
:- patch('noun','byline-91',(':ARG0':A0)^(':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)^(':ARG4':A4)^
                 ls(A0/ls(A0,":"),A1/ls(A1,","),A2/ls(A2,","),A3/ls(q("Trans:",A3)),
                    A4/ls(q("by"),A4))).
:- forall(member(W,['more','most','much','too','many']),
          (atom_string(W,WS),
          patch('adverb',W,('A':X)^(':op1':Y)^ls(X/advp(adv(WS),X),Y/advp(adv(WS),Y))))).
:- delete('noun','many').

% :- discontiguous noun/1,noun/2. % to remove spurious error messages during development
noun('more',('D':A)^('A':B)^np($A/d("the"),B,n("more"))).
noun('amr-choice',X):-conjunction('or',X).
noun('amr-unintelligible',(':value':V)^ls(V,q("(unintelligible)"))).
noun('amr-unknown',"*unknown*").
noun('amr-empty',q("*empty*")).
noun('contrast-01',(':ARG1':A1)^(':ARG2':A2)^s(A1,"but",A2)).% considered as noun in order to avoid insertion of that in processRole
noun('correlate-91',(':ARG1':A1)^(':ARG2':A2)^ls(A1,vp(v("correlate"),p("with"),A2))). % considered as noun 
noun('date-interval',(':op1':D1)^(':op2':D2)^pp(p("from"),D1,p("to"),D2)).
% distribution-range-91   typically used for normal distributions with mean and standard deviation
% frame arguments
% :ARG1 center, mean
% :ARG2 lower bound
% :ARG3 upper bound
% :ARG4 radius, distance from center to bounds
% :ARG5 confidence that value is in range, typically a percentage, e.g. 95%
% :ARG6 deviation covered by range, e.g. standard-deviation, standard-error-of-the-mean
% :ARG7 type of distribution, e.g. normal-distribution
noun('distribution-range-91',
     (':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)^(':ARG4':A4)^(':ARG5':A5)^(':ARG6':A6)^(':ARG7':A7)^
     ls(A7,A5/(pp(p("with"),A5,n("confidence"))*a(",")),A1,
        A4/ls(q("±"),A4),A2/pp(p("from"),A2),A3/pp(p("to"),A3),A6)).
noun('email-address-entity',(':value':V)^(ls(V)*tag("a",{"href":("mailto:"+V)}))).
noun('even-as',(':op1':O1)^ls(q("even"),q("as"),O1)).
noun('even-if',(':op1':O1)^ls(q("even"),q("if"),O1)).
noun('even-when',(':op1':O1)^ls(q("even"),q("when"),O1)).
noun('fluid-ounce',Ounce):-noun('ounce',Ounce).
noun('have-mod-91',(':ARG1':A1)^(':ARG2':A2)^(':degree':DEG)^ls(A1,v("be"),DEG,p("from"),A2)).
noun('have-org-role-91',(':ARG0':A0)^(':ARG2':A2)^(':ARG1':A1)^(':ARG3':A3)^
                         ls(A0/vp(A0,v("be")),A2,A1/pp(p("in"),A1),A3)).
noun('hyperlink-91',(':ARG3':A3)^A3).
noun('include-91',(':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)
                   ^ls(A2,vp(v("include"),A3/ls(A3,q("of")),A1))).
noun('infer-01',(':ARG1':A1)^(':ARG2':A2)^s(A1,"so",A2)). % considered as noun 
noun('instead-of-91',(':ARG1':A1)^(':ARG2':A2)^ls(A1,adv("instead"),p("of"),A2)).
noun('multi-sentence',(':snt1':S1)^(':snt2':S2)^(':snt3':S3)^(':snt4':S4)
                      ^(':snt5':S5)^(':snt6':S6)
                     ^ls((S1/s(S1)),(S2/(s(S2)*b(";"))),(S3/(s(S3)*b(";"))),
                         (S4/(s(S4)*b(";"))),(S5/(s(S5)*b(";"))),(S6/(s(S6)*b(";"))))).  
noun('more-than',(':op1':O1)^ls(q("more"),q("than"),O1)).
noun('natural-object',Object):-noun('object',Object).
noun('percentage-entity',(':value':V)^ls(V,q("percent"))).
noun('phone-number-entity',(':value':V)^ls("tel:",V)).
noun('rate-entity-91',(':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)^(':ARG4':A4)
                      ^ls(A1,A2/ls(p("per"),A2),A3/ls(q("every"),A3),A4)).
noun('ratio-of',(':op1':O1)^(':op2':O2)^ls(q("a ratio of"),O1,O2/pp(p("to"),O2))).
noun('relative-position',(':op1':O1)^(':direction':D)^(':quant':Q)^ls(Q,D/ls(p("to"),D),O1)).
noun('request-confirmation-91',q("is that right ?")).
noun('score-entity',(':op1':O1)^(':op2':O2)^ls(O1,q("to"),O2)).
noun('score-on-scale-91',(':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)
                          ^ls(A1/ls(q("score:"),A1),A2/pp(p("from"),A2),A3/pp(p("to"),A3))).
noun('several',(':op1':O1)^ls(q("several"),O1)).
noun('street-address-91',(':ARG1':A1)^(':ARG2':A2)^(':ARG3':A3)^(':ARG4':A4)^(':ARG5':A5)^(':ARG6':A6)
                         ^ls(A1,A2,A3,A4,A5,A6)). 
noun('string-entity',(':value':V)^(':mod':M)^ls(q(V)*en("\""),M)).
noun('this',d("this")).
noun('truth-value',(':polarity-of':A)^ls(q("whether"),A)).
noun('url-entity',(':value':V)^(V*tag("a",{"href":("http://"+V)}))).
noun('value-interval',(':op1':D1)^(':op2':D2)^ls(D1/pp(p("from"),D1),D2/pp(p("to"),D2))).

