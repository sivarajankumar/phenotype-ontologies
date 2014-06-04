# Makefile for phenotype OWL generation
#
# See README.md for details

# note:
# this Makefile will soon be replaced by the obo package manager
# https://docs.google.com/document/d/1VzlfrF29NpWonRLcUT3JZLEH5PoWa-wJxsrJstMKV8M/edit?authkey=COeH6vMP&hl=en

# ----------------------------------------
# VARIABLES
# ----------------------------------------

OBO=http://purl.obolibrary.org/obo
UPHENO = $(OBO)/upheno
ONTS = mp hp wbphenotype zp
ALL_ONTS = mp hp wbphenotype apo to
MP_URL = ftp://ftp.informatics.jax.org/pub/reports/MPheno_OBO.ontology

CATALOG = catalog-v001.xml
USECAT= --catalog-xml $(CATALOG)

# ----------------------------------------
# TOP LEVEL TARGETS
# ----------------------------------------

all: $(patsubst %,%-all,$(ONTS)) all_imports all_match_best_owl mammal-reasoned.owl vertebrate-reasoned.owl metazoa-reasoned.owl upheno.owl
all_obo : $(patsubst %,%.obo,$(ONTS))
all_xp_obo : $(patsubst %,%-equivalence-axioms.obo,$(ONTS))
all_owl : $(patsubst %,%.owl,$(ONTS))

%-all: %.obo %.owl
	make $*/$*-equivalence-axioms-subq-ubr.owl ONT=$*

clean:
	rm mp.obo hp.obo wbphenotype.owl

# ----------------------------------------
# PARSING, TRANSLATION AND REWRITING
# ----------------------------------------

refresh:
	yes p | svn update && mv mp.obo mp.obo.old && make mp.obo && mv hp.obo hp.obo.old && make hp.obo

# ~~~
# MP
# ~~~
# https://code.google.com/p/phenotype-ontologies/wiki/MP
mp.obo:
	wget $(MP_URL) -O $@
mp.owl: mp.obo
	owltools $< -o $@

# Note that MP now uses an OWL -edit file as source - the obo is generated from this;
# this reverses the standard direction of dependencies

#mp/mp-equivalence-axioms-subq-ubr.owl: mp/mp-equivalence-axioms-edit.owl
#	owltools $(USECAT) $< --remove-imports-declarations -o $@

#mp/mp-equivalence-axioms-subq-ubr.obo: mp/mp-equivalence-axioms-edit.owl
#	owltools $(USECAT) $< --add-support-from-imports --remove-imports-declarations -o -f obo $@
# TODO - required??
mp/mp-axiom-view.obo: mp/mp-edit.owl
	owltools --use-catalog $< -o -f obo $@

mp/mp-inferred.owl: mp.owl
	owltools --use-catalog --use-catalog mp/mp-importer.owl imports/grouping_phenotypes.owl --merge-support-ontologies --run-reasoner -r elk --assert-implied -o -f ofn $@.tmp && owltools $@.tmp -o -f ofn $@

# ~~~
# HP
# ~~~
# https://code.google.com/p/phenotype-ontologies/wiki/HP
hp.obo: external/hpo/human-phenotype-ontology.obo
	echo remember to update external/hpo && \
	cp $< $@
hp.owl: hp.obo
	owltools $< -o $@

# Mirror via svn:externals
hp/hp-axiom-view.obo: hp/hp-equivalence-axioms-subq-ubr.owl
	 owltools --use-catalog $< hp/hp-importer.owl -o -f obo $@

hp/hp-inferred.owl: hp.owl
	owltools --use-catalog --use-catalog hp/hp-importer.owl imports/grouping_phenotypes.owl --merge-support-ontologies --run-reasoner -r elk --assert-implied -o -f ofn $@.tmp && owltools $@.tmp -o -f ofn $@


# ~~~
# ZP
# ~~~
# https://code.google.com/p/phenotype-ontologies/wiki/ZP

## DO NOT UPDATE: https://code.google.com/p/bio-ontology-zp/issues/detail?id=4
#zp.owl:
#	owltools http://compbio.charite.de/hudson/job/hpo.ontology.uberpheno/lastStableBuild/artifact/data/dataForUberpheno/not_subsettable_ontologies/zp.owl --set-ontology-id $(OBO)/upheno/$@ -o $@
#zp.obo: zp.owl
#	owltools $< -o -f obo --no-check $@

zp/zp-mapping.txt: zp.owl
	owltools $< --extract-annotation-value --id-prefix ZP: -p http://zfin/definition/source_information -o $@

zp/zp-equivalence-axioms-subq.owl: zp.owl
	cp $< $@

zp/zp-equivalence-axioms-subq-ubr.owl: zp.owl uberon-simple-vertebrate-bridge.obo
	owltools $< uberon-simple-vertebrate-bridge.obo --add-imports-from-supports --merge-equivalent-classes -f ZFA  -t UBERON  --merge-equivalent-classes -f ZFA -t CL -o $@

zp/zp-inferred.owl: zp.owl
	owltools --use-catalog zp/zp-importer.owl imports/grouping_phenotypes.owl --merge-support-ontologies --run-reasoner -r elk --assert-implied -o -f ofn $@.tmp && owltools $@.tmp -o -f ofn $@


# ~~~
# FYPO
# ~~~
# TODO
fypo.owl:
	wget $(OBO)/fypo.owl
fypo.obo: fypo.owl
	owltools $< -o -f obo $@

# ~~~
# WBPhenotype
# ~~~
wbphenotype.owl:
	wget $(OBO)/wbphenotype.owl
wbphenotype.obo: wbphenotype.owl
	owltools $< -o -f obo --no-check $@

# Note that WPO now uses an OWL -edit file as source - the obo is generated from this;
# this reverses the standard direction of dependencies

wbphenotype/wbphenotype-equivalence-axioms-subq-ubr.owl: wbphenotype/wbphenotype-equivalence-axioms-edit.owl
	owltools $(USECAT) $< --remove-imports-declarations -o $@

wbphenotype/wbphenotype-equivalence-axioms-subq-ubr.obo: wbphenotype/wbphenotype-equivalence-axioms-edit.owl
	owltools $(USECAT) $< --add-support-from-imports --remove-imports-declarations -o -f obo $@

# ~~~
# Fly
# ~~~
dpo.owl:
	owltools $(OBO)/fbcv/dpo.owl --remove-axioms -t SubObjectPropertyOf --remove-annotation-assertions -l -d --set-ontology-id $(UPHENO)/$@ -o -f omn $@

imports/fbbt_import.owl:
	owltools $(OBO)/fbbt.owl --extract-mingraph --set-ontology-id $(UPHENO)/$@ -o -f omn $@.tmp && owljs-sed -o $@ 'function(ax,owl){if (owl.isLabelAssertion(ax) && ax.getSubject().toString().indexOf("FBbt") > -1) {return owl.labelAssertion(ax.getSubject(),"Drosophila "+ax.getValue().getLiteral())}}' $@.tmp

# ~~~
# Charite cross-species phenotype ontology
# ~~~
crossSpeciesPheno.obo:
	wget http://compbio.charite.de/hudson/job/hpo.ontology.uberpheno/lastStableBuild/artifact/data/uberphenoOut/crossSpeciesPheno.obo -O $@
crossSpeciesPheno.owl: upheno.obo
	owltools $< -o $@

# ~~~
# PATO
# ~~~
pato.obo:
	cp $(HOME)/cvs/obo/ontology/phenotype/quality.obo $@
pato.owl: pato.obo
	owltools $< -o $@



#@Deprecated?
###all_merged: uberpheno-subq-full-merged.owl

%-merged.owl: %.owl
	owltools --use-catalog $< --merge-import-closure --reasoner elk  --merge-equivalence-sets -s HP 9 -s MP 8 -s ZP 7 -l MP 9 -l HP 8 -l ZP 7 -d HP 9 -d MP 8 -d ZP 7 -o $@
##	owltools --use-catalog $< --merge-import-closure   --merge-equivalent-classes -f $(OBO)/MP_ -t $(OBO)/HP_ --merge-equivalent-classes -f $(OBO)/FMA_ -f $(OBO)/EMAPA_ -f $(OBO)/MA_ -t $(OBO)/UBERON_ --merge-equivalent-classes -f $(OBO)/FMA_ -t $(OBO)/CL_ -t $(OBO)/UBERON_ -o $@
%-merged.obo: %-merged.owl
	owltools $< -o -f obo --no-check $@.tmp && grep -v ^owl-axioms $@.tmp > $@

# ----------------------------------------
# Simple files
# ----------------------------------------

%-simple.owl: %.owl
	owltools  $(USECAT) --use-reasoner elk $< --merge-imports-closure --assert-inferred-subclass-axioms --markIsInferred --removeRedundant --allowEquivalencies -o $@.tmp --make-subset-by-properties -f // --extract-module  -d HP:0000118 MP:0000001  WBPhenotype:0000886  --set-ontology-id $(OBO)/$@ -o $@

%-simple.obo: %-simple.owl
	owltools $< -o -f obo --no-check $@.tmp && grep -v ^owl-axioms $@.tmp > $@

%-core.obo: %.owl
	owltools $(USECAT) $<  --add-support-from-imports -o -f obo --no-check $@

# ----------------------------------------
# Subqualities model
# ----------------------------------------
# (generic transform)
#
# Phenotype as a bag of qualities
#
# Here we transform 'Ph = Q and inheres_in some E' ==> 'Ph = has_part some (Q and inheres_in some E)'
# (or more generally, any such equivalence axiom)
# this better accounts for combined phenotypes, especially in HPO, and the fact that the PATO lineage often switches in the same phenotype hierarchy
# note: produces odd behavior with pseudo-anon classes, but these may be removed later.

%-subq.obo: %.obo
	../tools/make-subq-model-from-obo.pl $< > $@
%-subq.owl: %-subq.obo
	owltools $M -o $@

# ----------------------------------------
# Mapping IDs to Uberon
# ----------------------------------------
# things are much simpler if we use the same anatomy ontology
# for shared structures - we may also have ssAO classes as subclasses

# first we create a mapping file
# - this is a simplification of the actual taxon equiv axioms
UBRIDGE = external/uberon/bridge/uberon-bridge-to-

# hack: render taxon-equivalents as plain equivalents
#  NOTE: this could have unintended consequences and lead to unsatisfiable classes (e.g. check mammary gland)
uberon-simple-vertebrate-bridge.obo: 
	../tools/flatten-taxon-equivalents.pl --ont uberon/uberon-simple-vertebrate-bridge $(UBRIDGE)ma.obo $(UBRIDGE)fma.obo $(UBRIDGE)zfa.obo $(UBRIDGE)ehdaa2.obo external/uberon/bridge/cl-bridge-to-fma.obo > $@.tmp && mv $@.tmp  $@

# map IDs to uberon
# (note: when we switch to an ontology to editing OWL this will no longer be necessary)
%-ubr.obo: %.obo uberon-simple-vertebrate-bridge.obo
	owltools $< uberon-simple-vertebrate-bridge.obo --add-imports-from-supports --merge-equivalent-classes -f FMA -f MA -f ZFA  -t UBERON  --merge-equivalent-classes -f FMA -t CL -o -f obo --no-check $@.tmp && grep -v ^import $@.tmp > $@


# ----------------------------------------
# Module extraction / Imports generation
# ----------------------------------------
#
# We create shared modules for the combined set of phenotype
# ontologies
#
# Uses OWLAPI Module Extraction code
#
# Type 'make imports/X_import.owl' whenever you wish to refresh the import for an ontology X. This is when:
#
#  1. X has changed and we want to include these changes
#  2. We have added onr or more new IRI from X into any of the xp files
#
# You should NOT edit these files directly, changes will be overwritten.
#
# If you want to add something to these, edit the xp obo file and add an axiom with a IRI from X. You don't need to add any information about X.
#
# **Note that imports are shared by all projects**
# this means that any one import will be larger than is strictly required for any one species, but there are advantages to sharing here

# Base URI for local subset imports
##UPHENO = $(OBO)/upheno

# Ontology dependencies. TODO - nbo
IMPORTS = pato uberon chebi pr go doid mpath nbo

# Make this target to regenerate ALL
all_imports: $(patsubst %, imports/%_import.owl,$(IMPORTS))

KEEPRELS = BFO:0000050 BFO:0000051 RO:0002202 immediate_transformation_of

# Create an import module using the OWLAPI module extraction code via OWLTools.
# We use the standard catalog, but rewrite the import to X to be a local mirror of ALL of X.
# After extraction, we further reduce the ontology by creating a "mingraph" (removes all annotations except label) and by 

#imports/wbbt_import.owl: wbphenotype/wbphenotype-equivalence-axioms-subq-ubr.owl mirror/wbbt.owl
#	owltools  $(USECAT) --map-ontology-iri $(UPHENO)/wbbt_import.owl mirror/wbbt.owl $< --merge-imports-closure mirror/$*.owl --add-imports-from-support  --extract-module -s $(OBO)/$*.owl -c --remove-axiom-annotations --make-subset-by-properties $(KEEPRELS) --set-ontology-id $(UPHENO)/$@ -o $@


# GENERIC:
###imports/%_import.owl: metazoa.owl mirror/%.owl mp/mp-equivalence-axioms-edit.owl hp/hp-equivalence-axioms.obo zp.owl
imports/%_import.owl: imports/upheno-preimporter.owl mirror/%.owl mp.owl hp.owl zp.owl
	owltools  $(USECAT) --map-ontology-iri $(UPHENO)/$*_import.owl mirror/$*.owl $< --merge-imports-closure mirror/$*.owl --add-imports-from-support  --extract-module -s $(OBO)/$*.owl -c --remove-axiom-annotations --make-subset-by-properties -f $(KEEPRELS) -o $@.tmp && owltools $@.tmp --set-ontology-id $(UPHENO)/$@ -o $@

# clone remote ontology locally, perfoming some excision of relations and annotations
mirror/%.owl:
	owltools $(OBO)/$*.owl --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/$*.owl -o $@
# this is actually uberon+cl
mirror/uberon.owl: external/uberon/ext.owl
	owltools $(OBO)/uberon/ext.owl --merge-imports-closure --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties -n $(KEEPRELS)  --extract-mingraph --set-ontology-id $(OBO)/uberon.owl -o $@
##### need to sort sync issue
#####	owltools $(OBO)/uberon/ext.owl $(OBO)/cl.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/uberon.owl -o $@

mirror/wbbt.owl:
	owltools $(OBO)/uberon/basic.owl $(OBO)/cl.owl $(OBO)/wbls.owl $(OBO)/wbbt.owl $(OBO)/uberon/bridge/uberon-bridge-to-wbbt.owl $(OBO)/uberon/bridge/cl-bridge-to-wbbt.owl $(OBO)/ncbitaxon/subsets/taxslim.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS)  --set-ontology-id $(OBO)/wbbt.owl -o $@
###	owltools $(OBO)/uberon/ext.owl --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --set-ontology-id $(OBO)/uberon.owl -o $@
###	owltools  $(OBO)/wbls.owl $(OBO)/wbbt.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS)  --set-ontology-id $(OBO)/wbbt.owl -o $@
mirror/go.owl: 
	owltools $(OBO)/go/extensions/go-plus.owl $(OBO)/go/extensions/x-metazoan-anatomy.owl    --merge-imports-closure --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/go.owl -o $@
mirror/pr.obo:
	wget $(OBO)/pr.obo -O $@.tmp && obo-grep.pl -r 'id: PR:' $@.tmp > $@
mirror/pr.owl: mirror/pr.obo
	owltools $< --remove-dangling --set-ontology-id $(OBO)/pr.owl -o $@
.PRECIOUS: mirror/%.owl

imports/ro_import.owl: mirror/ro.owl
	owltools  $(USECAT) $< mp/mp-equivalence-axioms-edit.owl --extract-properties --set-ontology-id $(UPHENO)/upheno/$@ -o $@

imports/so_import.owl:
	owltools  $(USECAT) $< $(UPHENO)/data/human-genes.owl $(OBO)/so.owl --add-imports-from-supports --extract-module -s $(OBO)/so.owl -c --extract-mingraph --remove-annotation-assertions -l -d -s --make-subset-by-properties -f // --set-ontology-id $(UPHENO)/$@ -o $@

imports/%_import.obo: imports/%_import.owl
	owltools $< -o -f obo $@

external/uberon/%.owl:
	echo $@

external/mirror/%.owl:
	wget $(OBO)/$*.owl -O $@

# ----------------------------------------
# Genes
# ----------------------------------------
# @Deprecated - moved to data directory

DATA=../../data
MM=$(DATA)/Mus_musculus
mp/genes-as-phenotypes.owl:
	owltools --create-ontology $(UPHENO)/$@ --load-instances $(MM)/Mm-gene-to-phenotype-BF.txt --load-labels $(MM)/Mm-gene-labels.txt -o $@

# ----------------------------------------
# Builds for multi-species importers
# ----------------------------------------

main/metazoa.owl: metazoa.owl hp.owl mp.owl
	ontology-release-runner $(USECAT) --allow-overwrite --outdir main --no-subsets --reasoner elk --asserted --simple $<

%-reasoned.owl: %.owl hp.owl mp.owl
	owltools $(USECAT) $<  --merge-imports-closure --use-reasoner elk --assert-inferred-subclass-axioms --removeRedundant --markIsInferred --allowEquivalencies --set-ontology-id $(UPHENO)/$@ -o $@ 

%-reasoned.obo: %-reasoned.owl
	owltools $< -o $@


# ----------------------------------------
# AUTO QC
# ----------------------------------------
%-dv.txt: %.owl
	owltools $(USECAT) $< --merge-imports-closure  --reasoner elk --check-disjointness-axioms > $@
%-elk.rpt: %.owl
	owltools $(USECAT) $< --merge-imports-closure  --reasoner elk -u > $@

# ----------------------------------------
# SEMANTIC SIMILARITY
# ----------------------------------------

#PAIRS = mp/mp-to-hp wbphenotype/wbphenotype-to-mp fypo/fypo-to-mp
PAIRS = mp/mp-to-hp wbphenotype/wbphenotype-to-mp zp/zp-to-mp zp/zp-to-hp

all_match_best: $(patsubst %,%-match-best.tbl,$(PAIRS))
all_match_best_owl: $(patsubst %,%-match-best.owl,$(PAIRS))

BIGOWLTOOLS = OWLTOOLS_MEMORY=8G owltools

mp/mp-to-hp-match.tbl: mp.owl hp.owl
	$(BIGOWLTOOLS) $(USECAT) mammal.owl --make-default-abox --fsim-compare-atts -p mp/mp-to-hp-sim.properties -o $@.tmp && mv $@.tmp $@
mp/mp-to-hp-match-best.tbl: mp.owl hp.owl
	$(BIGOWLTOOLS) $(USECAT) mammal.owl --make-default-abox --fsim-att-top-simj -p mp/mp-to-hp-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
mp/mp-to-hp-match-best.owl:  mp.owl hp.owl
	$(BIGOWLTOOLS) $(USECAT) mammal.owl --make-default-abox --fsim-compare-atts -p mp/mp-to-hp-sim.properties --set bestOnly true --set scoringMetrics JACCARD --set outputFormat owl -o $@.tmp && mv $@.tmp $@



wbphenotype/wbphenotype-to-%-match.tbl: wbphenotype.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) metazoa.owl --make-default-abox --fsim-compare-atts -p wbphenotype/wbphenotype-to-$*-sim.properties -o $@.tmp && mv $@.tmp $@
wbphenotype/wbphenotype-to-%-match-best.tbl: wbphenotype.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) metazoa.owl --make-default-abox --fsim-att-top-simj -p wbphenotype/wbphenotype-to-$*-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
wbphenotype/wbphenotype-to-%-match-best.owl: wbphenotype.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) metazoa.owl --make-default-abox --fsim-compare-atts -p wbphenotype/wbphenotype-to-$*-sim.properties --set bestOnly true --set outputFormat owl -o $@.tmp && mv $@.tmp $@

zp/zp-to-%-match.tbl: zp.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) vertebrate.owl --make-default-abox --fsim-compare-atts -p zp/zp-to-$*-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
zp/zp-to-%-match-best.tbl: zp.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) vertebrate.owl --make-default-abox --fsim-att-top-simj -p zp/zp-to-$*-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
zp/zp-to-%-match-best.owl: zp.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) vertebrate.owl --make-default-abox --fsim-compare-atts -p zp/zp-to-$*-sim.properties --set bestOnly true --set outputFormat owl -o $@.tmp && mv $@.tmp $@

fypo/fypo-to-%-match.tbl: fypo.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) eukaryote.owl --make-default-abox --fsim-compare-atts -p fypo/fypo-to-$*-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
fypo/fypo-to-%-match-best.tbl: fypo.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) eukaryote.owl --make-default-abox --fsim-att-top-simj -p fypo/fypo-to-$*-sim.properties -o $@.tmp > $@.log && mv $@.tmp $@
fypo/fypo-to-%-match-best.owl: fypo.owl %.owl
	$(BIGOWLTOOLS) $(USECAT) eukaryote.owl --make-default-abox --fsim-compare-atts -p fypo/fypo-to-$*-sim.properties --set bestOnly true --set outputFormat owl -o $@.tmp && mv $@.tmp $@

DATA = ../../data
MM = $(DATA)/Mus_musculus
owlsim.cache: upheno.obo
	owltools $(USECAT)  upheno.obo --load-instances $(MM)/Mm-gene-to-phenotype-BF.txt --sim-compare-atts  -p config/owlsim-cache.properties -o $@.full --sim-save-lcs-cache $@
ic-cache.owl: upheno.obo
	owltools $(USECAT)  upheno.obo --load-instances $(MM)/Mm-gene-to-phenotype-BF.txt --no-debug --sim-save-ic-cache $@

# ~~~
# MP/VT
# ~~~
mp/mpdmap.csv:
	wget http://phenome.jax.org/download/ontology_mappings.csv -O $@
mp/mpdmap.tsv: mp/mpdmap.csv
	perl -npe 's/,/\t/g' $< > $@.tmp && cut -f1,2 $@.tmp > $@
mp/mpdmap.groups: mp/mpdmap.tsv
	blip-findall -r MA -r MP -r vt -i mp/mpdmap.tsv "setof(C,mpdmap(X,C),Cs),length(Cs,Len),Len>1" -select "x(X,Len,Cs)" -label -no_pred > $@

# ~~~
# MAPPING TO GO
# ~~~

# CRUDE
mp/mp-map2go.tbl: mp/mp-equivalence-axioms-subq-ubr.obo
	blip-findall -i $< -i mp.obo -r go_xp -r go_mg -r go_basic -consult util/gomap.pro ph_go_nr/4 -label -use_tabs -no_pred > $@.tmp && mv $@.tmp $@

# ~~~
# Generate labels (todo - use owltools)
# ~~~
%-names.obo: %.obo
	obo-filter-tags.pl  -t id -t name  $< > $@

# ----------------------------------------
# MERGED-SIMPLE
# ----------------------------------------
#...

# ----------------------------------------
# GROUPINGS
# ----------------------------------------
util/blacklist.js:
	owljs -i util/make-blacklist.js

imports/%_phenotype.owl: imports/%_import.owl util/blacklist.js 
	owljs -i util/make-grouping-classes.js

# ----------------------------------------
# DEFINITION PATTERNS
# ----------------------------------------
hp-patterns.txt: hp/hp-equivalence-axioms.obo
	blip-findall -r HP -consult util/pattern.pro -i $< pattern_xs/2 -no_pred -label | perl -npe 's/,HP:/\t/g' | sort -u > $@

# ----------------------------------------
# Monarch ontology inferences
# ----------------------------------------

# Note: currently monarch inferred contains all inferred direct links WITHIN the grouping ontology together with all inferred direct links
# BETWEEN {hp,zp,mp} classes and grouping ontology classes.
# Here the grouping phenotype ontology is the sum of imports/{uberon,go,cl}_phenotype
INFERRED = zp/zp-inferred.owl mp/mp-inferred.owl hp/hp-inferred.owl
imports/grouping_phenotype_inferences.owl: $(INFERRED)
	owltools $(INFERRED) --merge-support-ontologies -o -f ofn $@


# include hacky step for removing pseudo-anon classes for now - these should be removed from base ontology
#monarch-inferred-pre.owl: monarch.owl
#	owltools --use-catalog $< --remove-equivalent-to-nothing-axioms --remove-axioms -t DisjointClasses --remove-axioms -t ObjectPropertyDomain --remove-axioms -t ObjectPropertyRange --run-reasoner -r elk --assert-implied -o -f ofn $@
#monarch-inferred.owl: monarch-inferred-pre.owl
#	./util/hacky-filter-inferences.pl $< > $@
##	owltools --use-catalog $< --remove-axioms -t EquivalentClasses -o -f ofn $@
##	owltools --use-catalog $< --run-reasoner -r elk --assert-implied -o -f ofn $@.tmp && grep -v '_#_' $@.tmp > $@ && owltools --use-catalog $@



#%-merged.owl: %.owl
#	owltools --merge-imports-closure --use-catalog $< --run-reasoner -r elk --assert-implied --extract-mingraph --set-ontology-id $(UPHENO)/$@ -o -f omn $@
#%-merged.obo: %-merged.owl
#	owltools $< -o --merge-equivalence-sets -s HP 9 -s MP 8 -s ZP 1 -f obo --no-check $@

# ----------------------------------------
# ANALYSIS @Deprecated
# ----------------------------------------

all-by-all.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools phenotype-annotations.owl --merge $< --sim-method MultiSimilarity  --sim -p 'has phenotype' -a $(OBO)/NCBITaxon_10090 --vs $(OBO)/NCBITaxon_9606 --save-sim out.owl >& $@

all-ic.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools phenotype-annotations.owl --merge $< --all-class-ic > $@

all-closure.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools  phenotype-annotations.owl --merge $< --monitor-memory --save-closure $@

# ----------------------------------------
# obo2owl
# ----------------------------------------
# generic transform (typically overwritten)
#%.owl: %.obo
#	owltools $< -o $@

# ----------------------------------------
# stats
# ----------------------------------------
%.stats: %.owl
	owljs-owlstats -o $@ $<

# ----------------------------------------
# DEPLOY
# ----------------------------------------
# Temporary - covered by Jenkins in future
ARTEFACTS = 
deploy:
	rsync -avz $(ARTEFACTS) $(DEPLOY_TARGET)/

