## compile and install cgis for the AMR-Verbalizer web application

webpls := amrVerbalizer.pl amrGenerate.pl
cgis := $(patsubst %.pl,%.cgi,$(webpls))
cgi-dir := /Users/lapalme/Sites/gophi

info:
	echo "make install-cgi to install into the cgi dir"

## dependencies but not very useful (use touch-cgi)
amrGenerate.pl : inputPage.pl
	touch amrGenerate.pl

amrVerbalizer.pl: utils.pl checkParse.pl deepSyntR.pl surfSyntR.pl semantics.pl gophi.pl inputPage.pl \
                  deepSyntR.pl roles.pl pronounReference.pl specialConcept.pl \
                  dictionary.pl dictionaryGenerated.pl gender.pl dictionary-utils.pl
	touch amrVerbalizer.pl

## in principle, we should not have to create the ...2.pl file by piping 
##      tail -n +3 $^ | swipl -o $@ -g $ -t halt -c
##  but swipl does not seem to compile correctly in this case
%.cgi : %.pl
	tail -n +3 $^ > $(patsubst %.pl,%,$^)2.pl
	swipl -o $@ -g $(patsubst %.pl,%,$^) -t halt -c $(patsubst %.pl,%,$^)2.pl
	rm $(patsubst %.pl,%,$^)2.pl

touch-cgi:
	touch $(webpls)

install-cgi : $(cgis)
	cp $^ $(cgi-dir)

