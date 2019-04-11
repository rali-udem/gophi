# GoPhi : an AMR to ENGLISH VERBALIZER

GOPHI (*Generation Of Parenthesized Human Input*) is a system for generating a literal reading of Abstract Meaning Representation (AMR) structures. The system, written in [SWI-Prolog](http://www.swi-prolog.org "SWI-Prolog"), uses a symbolic approach to transform the original rooted graph into a tree of constituents that is transformed into an English sentence by [jsRealB](https://github.com/rali-udem/JSrealB "GitHub - rali-udem/JSrealB: A JavaScript bilingual text realizer for web development"). 

More information about the design and the rationale of the system [in this paper](documentation/GoPhi.pdf).

# Running GoPhi

## Using the web interface at RALI

* browse [GoPhi](http://rali.iro.umontreal.ca/amr/current/build/amrVerbalizer.cgi) to edit an AMR that can be verbalized

## Using the web interface with the internal SWI-Prolog web server
* Launch the SWI-Prolog console and type the commands at the `?-` prompt **not forgetting to end with a full stop**. Wait for the `true.` reply from the Prolog interpreter.

    * Load the system

        `cd('/path/to/the/build/directory').`  
        `[gophiWeb].`
    * In a web browser, browse : [http://127.0.0.1:8000/amrVerbalizer](http://127.0.0.1:8000/amrVerbalizer) to edit an AMR that can be verbalized

## From the SWI-Prolog console

* Launch the [jsRealB interpreter](https://github.com/rali-udem/JSrealB "GitHub - rali-udem/JSrealB: A JavaScript bilingual text realizer for web development") in another window/process  
`node /path/to/jsRealB/server-dme.js`
     
* Launch the SWI-Prolog console and type the commands at the `?-` prompt **not forgetting to end with a full stop**. Wait for the `true.` reply from the Prolog interpreter.

    * Load the system

        `cd('/path/to/the/build/directory').`  
        `[gophiFile].`

    * Verbalize a single AMR in a string (showing intermediary information).

        `testAMRstring(AMRstring).`  
        For example:   
        `testAMRstring('(s / say-01 :ARG0 (i/I) :ARG1 (h/hello) :ARG2 (w/world))').`
        
        it should return

            > Semantic Representation
            ['say-01',s,
                      [':ARG0',['I',i]],
                      [':ARG1',[hello,h]],
                      [':ARG2',[world,w]]]
            > Deep Syntactic Representation
            s(pro("I")*pe(1)*g("m")*n("s"),
              vp(v("say"),
                 np($(null)/d("the"),
                    null,
                    n("hello")),
                 non-null/pp(p("to"),
                             np($(null)/d("the"),
                                null,
                                n("world"))),
                 null/pp(p("of"),
                         null)))
            > Surface Syntactic Representation
            S(Pro("I").pe(1).g("m").n("s"),
              VP(V("say"),
                 NP(D("the"),
                    N("hello")),
                 PP(P("to"),
                    NP(D("the"),
                       N("world")))))
            > English sentence
            I say the hello to the world.

    * Show the input AMRs in a *file* that match a `Regex` (which can be '' to match all AMRs) and their English realization.

        `showRealisation(InFileName,Regex).`
        
        For example, to process AMRs with at least an inverse role:  
        `showRealisation('../../amr-examples/amr-examples.txt',':ARG.-of').`
        
    * Show the input AMRs in a *file* that match a `Regex` (which can be '' to match all AMRs) and display the intermediary structures leading to their English realization.

        `showAMRsFile(InFileName,Regex).`
        
        
# File organization of GoPhi

## Prolog (.pl) files (`build` directory)

### Transformations:
* AMR → Semantic Representation (SemR)
    * `checkParse.pl`    : parse with error messages in the case of a malformed AMR
    * `parse.pl`         : parse that takes for granted that AMR are well-formed, fails otherwise
    * `utils.pl`         : utilities for parsing and pretty-printing
    * `inverseRoles.pl`  : transformation of inverse roles in the SemR

* AMR → First-Order Logic 
    * `semantics.pl`     : transformation into FOL using the algorithm of Bos

* AMR → English
    * `baselineGen.pl`     : simplistic baseline generator
    
* Semantic Representation → Deep Syntactic Representation (DSyntR)
    * `deepSyntR.pl`           : basic transformations into DSyntR 
    * `pronounReference.pl`    : generate an appropriate pronominal reference 
    * `roles.pl`               : deal with most of the roles
    * `specialConcept.pl`      : deal with some special concepts

* Deep Syntactic Representation → Surface Syntactic Representation (SSurfR)
    * `surfSyntR.pl`           : DCG for creating a jsRealB expression

### Dictionaries
* `dictionary-utils.pl`    : auxiliary predicates for the dictionary
* `dictionaryGenerated.pl` : automatically generated dictionary using `tools/createPrologDico` that parses PropBank frames and add missing from the jsRealB lexicon 
* `dictionary.pl`          : additions and corrections to `dictionaryGenerated.pl`
* `gender.pl`: gender associated with some English words, created by `tools/addGender.py`

### Driving the whole process:
* `gophi.pl` : transforms a single AMR to an English sentence using the predicates:
    * `amr2SSyntR/4` is the main predicate for transformation
    * `parseFile/4` parses all AMRs in a file (with a possible selection via regex)
    * `jsRealB/2` either calls a server process or launches one at each call
    * `start/0` parses command line arguments to drive the transformation, calls the transformation, keeping full trace in `*.amr2engtrace`,  calls `tools/amrStats.py` to compute statistics and create an Excel file for development evaluation
* `gophiFile.pl` : Extracts AMR strings from a file in LDC format and generates an English sentence for each

### Web application
A CGI that creates a web page in which a user can edit an AMR, which is then transformed and realised by jsRealB in that same web page.

* `inputPage.pl`     : creates a web page with an embedded editor that contains an AMR with checkboxes for selecting the intermediary structures to show.
* `replyPage.pl` : creates a web page showing the original AMR, the selected representations and the English realization.
* `amrVerbalizer.pl` : shows the input page with either an initial AMR or the current one
* `amrGenerate.pl`   : parses the AMR (using `checkParse.pl`) if it detects errors it displays the input page with error messages; if there are no errors, then it realizes the AMR and displays the intermediary structures that the user has chosen when submitting the form.
* `gophi-web` directory
    * `addLexicon-dme.js` : local modifications to the jsRealB lexicon
    * `amr-verb.css` : CSS for the generated web pages
    * `amr-verb.js`  : javascript for use in the generated web pages
    * `jsRealB-dme.min.js` : minifyed jsRealB that generates from the SSurfR
* `makefile`         : compiles two CGis from `amrVerbalizer.pl` and `amrGenerate.pl` and moves them to a directory that an Apache server can use (compilation is not mandatory but it improves the efficiency of the system)
* `gophiWeb.pl`        : a version of the web application that uses the internal SWI-Prolog web server. It is appropriate for developing on a single machine, but at RALI we use the CGIs.

### Testing 
* `examples.pl` : a few examples as Prolog terms taken from articles and useful for testing without parsing a file
* `unitTests.pl` : a small set-up for testing, but not yet fully operational

## Informations (`documentation` directory)
* `GenAMR.pdf` : paper describing the rationale and design of `gophi`
* `ComparativeEvaluation.md` : *recipes* for getting and running three AMR generators and producing a spreadsheet for comparative evaluation

## Shell scripts (`scripts` directory)
* `gophiFile`  : calls `gophiFile.pl` with appropriate arguments
* `gophiDir`: calls `gophiFile` on all `amr*.txt` files in a given directory and keeps a copy of stdout in `*.amrtoengtrace` files

## Python files (`tools` directory)
**Caution**: some of these programs use files that are not available in this distribution because they are not in public domain (e.g. AMRs 2.0 from LDC) or that should be downloaded from the [AMR homepage](https://amr.isi.edu "Abstract Meaning Representation (AMR)").

* `addGender.py`: create `gender.pl` from `data/englishGenderWords.txt`
* `amrConceptsVSPropBankNLexicon.py` : compute statistics of occurrences of concepts found (or not) in PropBank
* `amrStats.py` : create Excel spreadsheet for *development* evaluation
* `amrStats.py`            : parses `.out` files, computes BLEU scores and creates an Excel file for manual evaluation
* `calculatebleu.py` : used by amrStats.py
* `createEvalFiles.py` : from a file containing many AMRs, generate a sample AMR file of appropriate format for `gophi`, `isiMT` and `jamr` [more info](documentation/ComparativeEvaluation.md)
* `createEvalSpreadsheet.py` : creates a comparative evaluation spreadsheet from the output of `gophi`, `isiMT` and `jamr` [more info](documentation/ComparativeEvaluation.md)
* `createPrologDico.py`    : parses PropBank frames and jsRealB lexicon to create `dictionaryGenerated.pl`

## AMR files
Text files containing AMRs for developing and testing in three directories:

* `abstract_meaning_representation_amr_2.0` : should be filled with content downloaded [from LDC](https://catalog.ldc.upenn.edu/LDC2017T10 "Abstract Meaning Representation (AMR) Annotation Release 2.0 - Linguistic Data Consortium").
* `amr-examples` : examples gathered for developing GOPHI:
    * amr-examples.txt : simple examples gathered from different papers
    * amr-dict-examples.txt : examples extracted from the [AMR dictionary](https://www.isi.edu/~ulf/amr/lib/amr-dict.html "AMR dict")
    * amr-guidelines-1_2_5.txt : examples extracted from the [AMR Guidelines](https://github.com/amrisi/amr-guidelines/blob/master/amr.md "amr-guidelines/amr.md at master · amrisi/amr-guidelines · GitHub")
* `amr-ISI` : examples to be downloaded from the [ISI download page](https://amr.isi.edu/download.html "Download &nbsp; Abstract Meaning Representation (AMR)")  
  
## Data file extensions
* `*.txt`                   : input AMRs
* `*.out`                   : input AMRs augmented with output of gophi and baselinegen
* `*.amr2engtrace`          : full trace of the output of the transformation of a .txt file
* `*.xlsx`                  : Excel file (AMR, Basegen, reference sent, gophi output) for development or comparative evaluation, conventionally we add information about the evaluator before `.xlsx`

[Guy Lapalme](lapalme@iro.umontreal.ca)