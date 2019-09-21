:-encoding(utf8).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% les exemples de l'article de Bos

ex(N,S):-ex(N,S,_).  % pour ignorer la phrase à générer

ex('0','(s / small :domain (m / marble) :polarity -)',
    "The marble is not small").
ex('1a','(e/moan-01 :ARG0 (x/child))',
   "a child moans").
ex('1c','(e/moan-01 :ARG0 (x/child) :polarity -)',
   "a child does not moan").
ex('1b','(e/give-01  ; a comment to ignore
             :ARG2 (y/child)
             :ARG0 (x/person :named "Ms Ribble")
             :ARG1 (z/envelope)); another comment at the end',
   "Ms Ribble gives an envelope to a child").
ex('1d','(e/give-01 
             :ARG0 (x/person :named "Ms Ribble")
             :ARG1 (z/envelope)
             :ARG2 (y/child)
             :polarity -)',
   "Ms Ribble does not give an envelope to the child").
ex('2',"(e/read-01 :ARG0 (x/girl) :ARG1 (y/book))",
   "a girl reads a book").
ex('2a',"(x/girl :ARG0-of (e/read-01 :ARG1 (y/book)))",
   "a girl reads a book").
ex('2aN',"(x/girl :ARG0-of (e/read-01 :ARG1 (y/book)) :polarity -)",
   "no girl reads a book").
ex('2b',"(y/book :ARG1-of (e/read-01 :ARG0 (x/girl)))",
   "a book that is read by a girl").
ex('2bN',"(y/book :ARG1-of (e/read-01 :ARG0 (x/girl) :polarity -))",
   "a book that is not read by a girl").
ex('3',"(e/shout-01 :ARG0 (x/teacher))",
   "a teacher shouts").
ex('4a',"(e/giggle-01 :polarity - :ARG0 (x/boy))",
    "a boy did not giggle").
ex('4b',"(x/language :domain-of (a / appropriate :polarity -))",
    "the language is not appropriate").
ex('4c',"(x/language :domain-of (a / appropriate))",
    "the language is appropriate").
ex('5a','(e/dry-02
             :ARG0 (x / person :named "Mr Krupp")
             :ARG1 x)',
    "Mr Krupp dries himself").
ex('5b','(w/want-01
             :ARG0 (g / person :named "George")
             :ARG1 (p/play-01 :ARG3 g))',
    "George wants to play").
ex('9a','(x / boy :polarity -
            :ARG0-of (e / whistle-01
                          :polarity -))',
   "every boy whistle").

% exemple de https://github.com/amrisi/amr-guidelines/blob/master/amr.md#abstract-meaning-representation-amr-10-specification
ex('AMR-1',
   '(w / want-01
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :ARG0 (g / girl)
               :ARG1 b))',
  "The boy desires the girl to believe him.").
ex('AMR-1N',
   '(w / want-01
         :polarity -
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :ARG0 (g / girl)
               :ARG1 b))',
  "The boy does not desire the girl to believe him.").
ex('AMR-1NN',
   '(w / want-01
         :ARG0 (b / boy)
         :ARG1 (b2 / believe-01
               :polarity -
               :ARG0 (g / girl)
               :ARG1 b))',
  "The boy desires the girl not to believe him.").

ex('Guy', % paper example
   '(d / desire-01
        :ARG0 (b/boy)
        :ARG1 (g/girl
                 :ARG0-of (l/like-01
                             :polarity - 
                             :ARG1 b)))',
   "The boy desires the girl who does not like him").

% exemple de https://amr.isi.edu/language.html
ex('AMR-2',
   '(w / want-01
          :ARG0 (b / boy)
          :ARG1 (g / go-01
                :ARG0 b))',
   "The boy wants to go").
ex('AMR-3',
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
            :mod (w3 / web)))',
   "About 14,000 people fled their homes at the weekend after
    a local tsunami warning was issued, the UN said on its Web site").

%  http://nlp.arizona.edu/SemEval-2017/pdf/SemEval090.pdf
ex('SemEval-1',     % Figure 1
   '(f / fear-01
     :polarity -
     :ARG0 ( s / soldier )
     :ARG1 ( d / die-01
             :ARG1 s ))',
   "The soldier did not fear dying").

ex('SemEval-3',     % Figure 3
   '(c/claim-01
       :ARG0 (h/he)
       :ARG1 (e/expose-01
                :ARG0 (p/person
                         :ARG0-of (s/sing-01)
                         :age (t/ temporal-quantity :quant 28
                                    :unit (y/year)))
                :ARG1 p
                :ARG2 h
                :ARG1-of (r/repeat-01)))',
   "he claims the 28-year-old singer repeatedly exposed herself to him").

ex('SemEval-5',    % Table 5
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
                          :op2 (d4 / date-entity :year 1995)))))',
   "following the 1992-1995 war, bosnia remains ethnically divided and
    violence during major football matches occasionally occurs here.").

%  AMR tutorial
ex('Tut-01',    % page 6
   '(e / eat-01
     :ARG0 (d / dog)
     :ARG1 (b / bone))',
   "The dog is eating a bone").
ex('Tut-02a',   % page 10 Tut-02a et Tut-02b donnent la meme s�mantique
   '(w / want-01
      :ARG0 (d / dog)
      :ARG1 (e / eat-01
             :ARG0 d
             :ARG1 (b / bone)))',
   "The dog wants to eat the bone").
ex('Tut-02b',
   '(w / want-01
      :ARG0 d
      :ARG1 (e / eat-01
             :ARG0 (d / dog)
             :ARG1 (b / bone)))',
   "The dog wants to eat the bone").
ex('Tut-03',   % page 22
   '(e / eat-01
      :ARG0 (d / dog)
      :ARG1 (b / bone
             :ARG1-of (f / find-01
                         :ARG0 d)))',
   "The dog ate the bone that he found").

% Konstas et al. Neural AMR: Sequence-to-Sequence Models for Parsing and Generation, p
% http://aclweb.org/anthology/P17-1014
% output from 
ex('Konstas-F1',
   '(a / and 
      :op1 (e / elect-01 
             :ARG0 (p / person 
                      :name "Obama"))
      :op2 (c / celebrate-01 
             :ARG0 (p1 / person
                     :poss p
                     :ARG0-of (v / vote-01))))',
   "Obama was elected and his voters celebrated").

ex('Konstas-F2',
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
                              :op2 "York")))',
    "US officials held an expert group meeting in January 2002 in New York.").

%% output of Konstas:
%%  the arms control treaty limits the number of conventional weapons that can be deployed west of Ural Mountains .
%% we "delinearize" the examples of Figure 3 given in the paper... and add frame numbers
ex('Konstas-F3a',
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
                                    :ARG1-of (p / possible-01)))))',
    "The arms control treaty limits the number of conventional weapons that can be deployed west of the Ural Mountains.").
    
%% taken from AMR Normalisation for Fairer Evaluation
%%  other examples in reification.pl
ex('Goodman-F5','
(b/bite-01
  :ARG0 (d/dog
     :ARG0-of (c/chase-01
         :ARG1 (b1/boy)))
  :ARG1 b1)
',"The dog chasing the boy bit him.").

ex('Goodman-F6','
(b/bite-01
  :ARG0 d
  :ARG1 (b1/boy
     :ARG1-of (c/chase-01
          :ARG0 (d/dog))))
',"The boy chased by the dog was bit by it.").

ex('Goodman-F6b','
(b/bite-01
  :ARG0 (d/dog)
  :ARG1 (b1/boy
     :ARG1-of (c/chase-01
          :ARG0 d)))
',"The boy chased by the dog was bit by it.").
