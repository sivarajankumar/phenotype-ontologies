all: uberpheno

# uses the upheno_maker.pro script to build uberpheno (requires Thea)
uberpheno: mp-hp-ext-merged-uberon.owlpl zpheno.owl
	 thea-jpl --jvm-opt -Xmx4G --debug upheno $< zpheno.owl  --consult ../../tools/upheno_maker/upheno_maker.pro --goal make_upheno >& $@.LOG

#uberpheno: 
#	 thea-jpl --debug upheno --format rdf_direct file:../hp-mp/mp-hp-ext-merged-uberon.owl  --consult ../../tools/upheno_maker/upheno_maker.pro --goal make_upheno >& $@.LOG

# makes indexed version of OWL file (faster)
%.owlpl: %.owl
	thea $< --to owlpl > $@

# for now we get the pre-built file which is stored on phenotype-commons.
#  you can build this file yourself; see the Makefile in the parent directory
mp-hp-ext-merged-uberon.owl:
	wget http://obo.svn.sourceforge.net/viewvc/obo/phenotype-commons/ontology/mp-hp-ext-merged-uberon.owl

zpheno.owl:
	wget http://obo.svn.sourceforge.net/viewvc/obo/phenotype-commons/annotations/ZFIN/gene_phenotype.owl -O $@


mktest: 
	 thea-jpl --debug upheno --format rdf_direct file:test/kyph.owl --consult ../../tools/upheno_maker/upheno_maker.pro --goal make_upheno >& $@.LOG

