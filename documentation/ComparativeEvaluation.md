# Comparative evaluation of 3 AMR generators: 
#    gophi, isiMT, JAMR

### File name *variables*
`ROOT`=`/Users/lapalme/Dropbox/PourOrigene/AMR`  
`AMRS1=$ROOT/abstract_meaning_representation_amr_2.0/data/amrs/split/test`  
`AMRS2=$ROOT/amr-ISI`  
`TEST=test-all`  
`JAMR=/Users/lapalme/Desktop/jamr`
`JSREALB=/Users/lapalme/Documents/GitHub/jsRealB/build/server-dme.js`

**CAUTION**:When launching programs, it is more *safe* to specify full path names for files

## AMRs sampling

* create `$TEST` a single file containing AMRs e.g. by concatenating many `*.txt`
* `cd $ROOT/gophi/`
* `tools/createEvalFiles.py ${TEST}.txt N` where  
	* `N` is the sample size
* This will create three directories containing each a `${TEST}-N.txt` with the same AMRs but in the appropriate for each generator
	* `$AMRSi/eval/gophi` 
	* `$AMRSi/eval/isiMT` 
	* `$AMRSi/eval/jamr` 

## Running each generator

### gophi
* Launch `jsRealB` (in a separate window/process)
    * `node $JSREALB`

* Generate verbalisations
    * `cd $ROOT/gophi/scripts`
    * `./gophiFile $AMRSi/eval/gophi/${TEST}-N.txt`  
    to create
    	* `$AMRSi/eval/gophi/${TEST}-N.out`: original file *enriched* with output of **gophi** and of the *baseline generator* 
    	* `$AMRSi/eval/gophi/${TEST}-N.xlsx`: Excel file for the development evaluation (0-4 scale)  **gophi** output

### [ISIMT](https://www.isi.edu/projects/nlg/software_1 "Software")
* copy `AMRSi/eval/isiMT/${TEST}-N.txt` on a DIRO account (here `$DIRO/${TEST}-N.txt`)
* log on `octal15.iro.umontreal.ca` (possibly via `arcade.iro.umontreal.ca`)
* launch `/u/gottif/proj/misc/amr/src/amr2eng/run.sh < $DIRO/${TEST}-N.txt > $DIRO/${TEST}-N.out`.  
 **DO NOT** forget redirections otherwise the program stalls on `stdin`...
* copy `$DIRO/${TEST}-N.out` on `$AMRSi/eval/isiMT/${TEST}-N.out`  

### [JAMR](https://github.com/jflanigan/jamr "GitHub - jflanigan/jamr: JAMR Parser and Generator")
* `cd $JAMR`
* `. scripts/config_LDC2014T12-NAACL2016-generator.sh`
* `scripts/GENERATE.sh $AMRSi/eval/jamr/${TEST}-N.txt`  
to create
	* `$AMRSi/eval/jamr/${TEST}-N.txt.out` : generated sentences (watch out for the 2 extensions)	
	* `$AMRSi/eval/jamr/${TEST}-N.txt.err` : execution trace (watch out for the 2 extensions)  
	NB: during execution, other files and directories are created but deleted if all ends well...

## Generate the comparative evaluation file
* `cd $ROOT/gophi/`
* `tools/createEvalSpreadsheet.py $AMRSi/eval/gophi/${TEST}-N.out` (do not take `.txt`!) to create `$AMRSi/eval/${TEST}-N.xlsx` combining the output of the three systems into a single evaluation file.

## Manual evaluation 
* load `$AMRSi/eval/${TEST}-N.xlsx` in Excel
* add `X` or anything else in the line corresponding to the  *best* sentence. It is allowed to pick more than one good sentence, but at east one *less bad* sentence for each AMR. Statistics are automatically tallied at the bottom of the sheet. We take for granted that the evaluator does not display the hidden columns which identify the system for each sentence.

[Guy Lapalme](lapalme@iro.umontreal.ca) 