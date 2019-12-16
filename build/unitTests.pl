:- encoding(utf8).
%%  some unit tests 
%%  CAUTION: this relies on launching a node process at each test given by the two first args of jsRealBfilter
%%     this path should be fixed in the call within fullGen/2
%%   for making all tests
%%     :- [gophiFile,unitTests].
%%     :- testAll.

fullGen(AMRstring,GenSent):-
    amr2SSyntR(AMRstring,SSyntR,false,false),
    jsRealBfilter('/usr/local/bin/node','./jsRealB-filter.js',
                  SSyntR,GenSent).

%% useful for creating the expected generated sentence with the current version
%%  which can be copied into this program...
showExpected(Id,S) :-
    fullGen(S,GenSent),
    format('expected(
    ~q,
    ~q,
    \'~s\').~n',[Id,GenSent,S]).
    
showExpectedEx:- forall(ex(Id,S),showExpected(Id,S)).

test(Id,ExpectedGen,AMRstring):-
    fullGen(AMRstring,GenSent),
    (GenSent=ExpectedGen -> 
        writeln(Id:"success");
     writeln(Id:"failure"),
     writeln(AMRstring),
     writeln("Gen":GenSent),
     writeln("Exp":ExpectedGen),
     writeln("---"),
     fail).

testAll :- 
    aggregate_all(count,expected(_,_,_),Total),
    aggregate_all(count,(expected(Id,ExpectedGen,AMRString),test(Id,ExpectedGen,AMRString)),NB),
    Rate is NB*100 div Total,
    format("~d successes over ~d tests (~d %)~n",[NB,Total,Rate]).
        
%%% expected pairs AMR=Generated sentence

expected(
  'Paper',
  "The boy desires the girl who does not like him.",
  '(d / desire-01
        :ARG0 (b/boy)
        :ARG1 (g/girl
                 :ARG0-of (l/like-01
                             :polarity - 
                             :ARG1 b)))').
expected(
  '0',
  "Small is not the marble.",
  '(s / small :domain (m / marble) :polarity -)').
expected(
  '1a',
  "The child moans.",
  '(e/moan-01 :ARG0 (x/child))').
expected(
  '1c',
  "The child does not moan.",
  '(e/moan-01 :ARG0 (x/child) :polarity -)').
expected(
  '1b',
  "Ms Ribble gives the envelope to the child.",
  '(e/give-01  ; a comment to ignore
             :ARG2 (y/child)
             :ARG0 (x/person :named "Ms Ribble")
             :ARG1 (z/envelope)); another comment at the end').
expected(
  '1d',
  "Ms Ribble does not give the envelope to the child.",
  '(e/give-01 
             :ARG0 (x/person :named "Ms Ribble")
             :ARG1 (z/envelope)
             :ARG2 (y/child)
             :polarity -)').
expected(
  '2',
  "The girl reads the book.",
  '(e/read-01 :ARG0 (x/girl) :ARG1 (y/book))').
expected(
  '2a',
  "the girl who reads the book",
  '(x/girl :ARG0-of (e/read-01 :ARG1 (y/book)))').
expected(
  '2aN',
  "no girl who reads the book",
  '(x/girl :ARG0-of (e/read-01 :ARG1 (y/book)) :polarity -)').
expected(
  '2b',
  "the book that the girl reads",
  '(y/book :ARG1-of (e/read-01 :ARG0 (x/girl)))').
expected(
  '2bN',
  "the book that the girl does not read",
  '(y/book :ARG1-of (e/read-01 :ARG0 (x/girl) :polarity -))').
expected(
  '3',
  "The teacher shouts.",
  '(e/shout-01 :ARG0 (x/teacher))').
expected(
  '4a',
  "The boy does not giggle.",
  '(e/giggle-01 :polarity - :ARG0 (x/boy))').
expected(
  '4b',
  "the language that is not appropriate",
  '(x/language :domain-of (a / appropriate :polarity -))').
expected(
  '4c',
  "the appropriate language",
  '(x/language :domain-of (a / appropriate))').
expected(
  '5a',
  "Mr Krupp dries himself.",
  '(e/dry-02
             :ARG0 (x / person :named "Mr Krupp")
             :ARG1 x)').
expected(
  '5b',
  "George wants to play against him.",
  '(w/want-01
             :ARG0 (g / person :named "George")
             :ARG1 (p/play-01 :ARG3 g))').
expected(
  '9a',
  "no boy who does not whistle",
  '(x / boy :polarity -
            :ARG0-of (e / whistle-01
                          :polarity -))').
expected(
  'AMR-1',
  "The boy wants that the girl believes him.",
  '(w / want-01
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :ARG0 (g / girl)
               :ARG1 b))').
expected(
  'AMR-1N',
  "The boy does not want that the girl believes him.",
  '(w / want-01
         :polarity -
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :ARG0 (g / girl)
               :ARG1 b))').
expected(
  'AMR-1NN',
  "The boy wants that the girl does not believe him.",
  '(w / want-01
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :polarity -
               :ARG0 (g / girl)
               :ARG1 b))').
expected(
  'AMR-2',
  "The boy wants that he goes.",
  '(w / want-01
          :ARG0 (b / boy)
          :ARG1 (g / go-02
                :ARG0 b))').
expected(
  'AMR-3',
  "UN says that about 14000 persons flee his home the weekend after the tsunami is warned in the local in its web site.",
  '(s / say-01
     :ARG0 (g / organization
          :name (n / name
                  :op1 "UN"))
     :ARG1 (f / flee-05
          :ARG0 (p / person
                  :quant (a / about
                           :op1 14000))
          :ARG1 (h / home
                  :poss p)
          :time (w / weekend)
          :time (a2 / after
                  :op1 (w2 / warn-01
                         :ARG1 (t / tsunami)
                         :location (l / local))))
      :medium (s2 / site
            :poss g
            :mod (w3 / web)))').
expected(
  'SemEval-1',
  "The soldier does not fear the death.",
  '(f / fear-01
     :polarity -
     :ARG0 ( s / soldier )
     :ARG1 ( d / die-01
             :ARG1 s ))').
expected(
  'SemEval-3',
  "He claims that the singer 28 years old exposes to him himself that is repeated.",
  '(c/claim-01
       :ARG0 (h/he)
       :ARG1 (e/expose-01
                :ARG0 (p/person
                         :ARG0-of (s/sing-01)
                         :age (t/ temporal-quantity :quant 28
                                    :unit (y/year)))
                :ARG1 p
                :ARG2 h
                :ARG1-of (r/repeat-01)))').
expected(
  'SemEval-5',
  "<a href=\"https://en.wikipedia.org/wiki/Bosnia_and_Herzegovina\">Bosnia</a> remains under that it is divided about ethnic, the occasional violence the football match that is major in here and when follows the war from 1992 to 1995",
  '(a / and
     :op1 (r / remain-01
           :ARG1 (c / country :wiki "Bosnia_and_Herzegovina"
                  :name (n / name :op1 "Bosnia"))
           :ARG3 (d / divide-02
                  :ARG1 c
                  :topic (e / ethnic)))
     :op2 (v / violence
           :time (m / match-03
                  :mod (f2 / football)
                  :ARG1-of (m2 / major-02))
           :location (h / here)
           :frequency (o / occasional))
     :time (f / follow-01
            :ARG2 (w / war-01
                   :time (d2 / date-interval
                          :op1 (d3 / date-entity :year 1992)
                          :op2 (d4 / date-entity :year 1995)))))').
expected(
  'Tut-01',
  "The dog eats the bone.",
  '(e / eat-01
     :ARG0 (d / dog)
     :ARG1 (b / bone))').
expected(
  'Tut-02a',
  "The dog wants that it eats the bone.",
  '(w / want-01
      :ARG0 (d / dog)
      :ARG1 (e / eat-01
             :ARG0 d
             :ARG1 (b / bone)))').
expected(
  'Tut-02b',
  "It wants that the dog eats the bone.",
  '(w / want-01
      :ARG0 d
      :ARG1 (e / eat-01
             :ARG0 (d / dog)
             :ARG1 (b / bone)))').
expected(
  'Tut-03',
  "The dog eats the bone that it finds.",
  '(e / eat-01
      :ARG0 (d / dog)
      :ARG1 (b / bone
             :ARG1-of (f / find-01
                         :ARG0 d)))').
expected(
  'Konstas-F1',
  "Obama elects and his voter celebrates",
  '(a / and 
      :op1 (e / elect-01 
             :ARG0 (p / person 
                      :name "Obama"))
      :op2 (c / celebrate-01 
             :ARG0 (p1 / person
                     :poss p
                     :ARG0-of (v / vote-01))))').
expected(
  'Konstas-F2',
  "The person who is the official in United States holds that the person who is expert whom groups under him meets on January 2002 in New York.",
  '(h / hold-04
       :ARG0 (p2 / person
                 :ARG0-of (h2 / have-org-role-91 
                       :ARG1 (c2 / country
                                :name (n3 / name
                                         :op1 "United" 
                                         :op2 "States"))
                       :ARG2 (o / official))) 
       :ARG1 (m / meet-03
               :ARG0 (p / person
                         :ARG1-of (e / expert-01)
                         :ARG2-of (g / group-01)))
       :time (d2 / date-entity :year 2002 :month 1)
       :location (c / city
                    :name (n / name 
                              :op1 "New" 
                              :op2 "York")))').
expected(
  'Konstas-F3a',
  "The treaty that controls about the arms limits that the weapon that is conventional that is deployed to Ural Mountains to west that it is possible numbers.",
  '(l/limit-01
       :ARG0 (t / treaty 
               :ARG0-of (c / control-01 :ARG1 (a/arms)))
       :ARG1 (n / number-01
                :ARG1 (w / weapon 
                         :mod conventional
                         :ARG1-of (d / deploy-01
                                    :ARG2 (r / relative-position 
                                              :op1 "Ural Mountains" 
                                              :direction "west" )
                                    :ARG1-of (p / possible-01)))))').

expected(
    'isi_0002.376',
    "I cannot work in the home because she shouts at me.",
    '(p / possible-01
     :polarity -
     :ARG1 (w / work-01
          :ARG0 (i / i)
          :location (h / home))
     :ARG1-of (c / cause-01
          :ARG0 (s / shout-01
               :ARG0 (s2 / she)
               :ARG2 i)))').

expected(
    'isi_0002.101',
    "They fear him even when he is imprisoned.",
    '(f / fear-01
     :ARG0 (t / they)
     :ARG1 (h / he)
     :concession (e / even-when
          :op1 (i / imprison-01
               :ARG1 h)))').

expected(
    'isi_0002.324',
    "Up to 853 passengers are seated in <a href=\"https://en.wikipedia.org/wiki/Airbus_A380\">Airbus A380.</a>",
    '(s / seat-01
     :ARG1 (p / passenger
          :quant (u / up-to
               :op1 853))
     :location (a / aircraft-type
          :wiki "Airbus_A380"
          :name (n / name
               :op1 "Airbus"
               :op2 "A380")))'
).