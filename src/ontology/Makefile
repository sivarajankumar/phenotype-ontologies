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
ONTS = mp hp
ALL_ONTS = mp hp wbphenotype apo to
MP_URL = ftp://ftp.informatics.jax.org/pub/reports/MPheno_OBO.ontology
WP_URL = http://tazendra.caltech.edu/~azurebrd/cgi-bin/forms/phenotype_ontology_obo.cgi

CATALOG = catalog-v001.xml

# ----------------------------------------
# TOP LEVEL TARGETS
# ----------------------------------------

all: $(patsubst %,%-all,$(ONTS))
all_obo : $(patsubst %,%.obo,$(ONTS))
all_xp_obo : $(patsubst %,%-equivalence-axioms.obo,$(ONTS))
all_owl : $(patsubst %,%.owl,$(ONTS))

%-all:
	make $*/$*-equivalence-axioms.owl $*/$*-equivalence-axioms-tr.owl $*/$*-ext-merged-tr.owl ONT=$*

# ----------------------------------------
# PARSING, TRANSLATION AND REWRITING
# ----------------------------------------

fetch:
	svn update && mv mp.obo mp.obo.old && make mp.obo && mv hp.obo hp.obo.old && make hp.obo

mp.obo:
	wget $(MP_URL) -O $@

wbphenotype.obo:
	wget $(WP_URL) -O $@

hp.obo: external/hpo/human-phenotype-ontology.obo
	echo remember to update external/hpo && \
	cp $< $@

pato.obo:
	cp $(HOME)/cvs/obo/ontology/phenotype/quality.obo $@

# todo - syntactic check
#mp/mp-equivalence-axioms.obo: edit/mp-equivalence-axioms.obo
#	cp $< $@

# todo - syntactic check
hp/hp-equivalence-axioms.obo: external/hpo/human-phenotype-ontology_xp.obo
	cp $< $@

%.owl: %.obo
	owltools  $< -o file://`pwd`/$@
#	obolib-obo2owl --allow-dangling --default-ontology $*  $< -o $@

%.owl.owlpl: %.owl
	thea --to owlpl $< > $@

hp/hp-ext-merged.owl: hp.owl hp/hp-equivalence-axioms.owl
	owltools file:$< --merge file:hp/hp-equivalence-axioms.owl -o file://`pwd`/$@
mp/mp-ext-merged.owl: mp.owl mp/mp-equivalence-axioms.owl
	owltools file:$< --merge file:mp/mp-equivalence-axioms.owl -o file://`pwd`/$@

hp/hp-defs-simple.owl: hp/hp-equivalence-axioms.obo
	thea phenorel.owl --popl-file ../tools/model-translation/tr.popl $< --to rdf_direct > $@
#	thea phenorel.owl --popl-file ../tools/model-translation/tr.popl into "http://purl.obolibrary.org/obo/hp/hp-defs-simple.owl" $< --to rdf_direct > $@

# phenotype simple model (experimental)
#hp/hp-ext-merged-psm.owl: hp-psm.owl hp/hp-equivalence-axioms-psm.owl
#	owltools file:$< --merge file:hp/hp-equivalence-axioms-psm.owl -o file://`pwd`/$@
#mp/mp-ext-merged-psm.owl: mp-psm.owl mp/mp-equivalence-axioms-psm.owl
#	owltools file:$< --merge file:mp/mp-equivalence-axioms-psm.owl -o file://`pwd`/$@

$(ONT)/%-ext-merged.owl: %.owl $(ONT)/%-equivalence-axioms.owl
	owltools file:$< --merge file:$(ONT)/$*-equivalence-axioms.owl -o file://`pwd`/$@

# merge in the rewritten axioms
$(ONT)/%-ext-merged-tr.owl: %.owl $(ONT)/%-equivalence-axioms-tr.owl
	owltools --create-ontology $(OBO)/$@ --merge file:$< --merge file:$(ONT)/$*-equivalence-axioms-tr.owl -o file://`pwd`/$@

# apply POPL transform to rewrite expressions in axiom
%-tr.owl: %.owl
	thea --popl-file std_to_pheno.popl $< --to owl > $@

# alternate translation to frames
#%-fr.owl: %.owl
#	thea --debug popl --popl-file std_to_frames.popl $< --to owl > $@

# phenotype simple model (psm)
#%-psm.obo: %.obo psm/psm.obo
#	../tools/converters/std-to-frames.pl $< psm/psm.obo > $@
#.PRECIOUS: %-psm.obo

# new
all_merged: uberpheno-subq-full-merged.owl
%-merged.owl: %-importer.owl mp/mp-equivalence-axioms-subq.owl hp/hp-equivalence-axioms-subq.owl mp.owl hp.owl
	owltools --use-catalog $< --merge-import-closure --ni graph-traversal-metadata.owl --merge-support-ontologies   --merge-equivalent-classes -f $(OBO)/MP_ -t $(OBO)/HP_ --merge-equivalent-classes -f $(OBO)/FMA_ -f $(OBO)/EMAPA_ -f $(OBO)/MA_ -t $(OBO)/UBERON_ --merge-equivalent-classes -f $(OBO)/FMA_ -t $(OBO)/CL_ -t $(OBO)/UBERON_ -o file://`pwd`/$@

# ----------------------------------------
# Subqualities model
# ----------------------------------------
%-subq.obo: %.obo
	../tools/make-subq-model-from-obo.pl $< > $@


# temp hack:
fixed-behavior_xp.obo: external/behavior-ontology/behavior_xp.obo
	obo-grep.pl --neg -r PATO $< > $@

# ----------------------------------------
# Uberon
# ----------------------------------------

%-ubr.obo: %.obo uberon-simple-vertebrate-bridge.obo
	owltools $< uberon-simple-vertebrate-bridge.obo --add-imports-from-supports --merge-equivalent-classes -f FMA -f MA -f ZFA  -t UBERON  --merge-equivalent-classes -f FMA -t CL -o -f obo --no-check $@.tmp && grep -v ^import $@.tmp > $@

# mapping to uberon
# TODO: don't hardcode idspace
#%-ubr.obo: %.obo
#	obo-map-ids.pl --use-xref-inverse --xref-idspace MA $(HOME)/cvs/uberon/uberon_edit.obo $< > $@



UBRIDGE = external/uberon/bridge/uberon-bridge-to-

# hack: render taxon-equivalents as plain equivalents
#  NOTE: this could have unintended consequences and lead to unsatisfiable classes (e.g. check mammary gland)
uberon-simple-vertebrate-bridge.obo: external/uberon/uberon.obo
	../tools/flatten-taxon-equivalents.pl --omt uberon/uberon-simple-vertebrate-bridge $(UBRIDGE)ma.obo $(UBRIDGE)fma.obo $(UBRIDGE)zfa.obo $(UBRIDGE)ehdaa2.obo external/uberon/bridge/cl-bridge-to-fma.obo > $@.tmp && mv $@.tmp  $@


# ----------------------------------------
# Oort
# ----------------------------------------
OBO=http://purl.obolibrary.org/obo
#CORE= external/pato/quality.obo $(OBO)/go.owl external/cell/cl.owl $(OBO)/uberon/merged.owl $(OBO)/nbo.owl $(OBO)/mpath.owl
CORE= external/pato/quality.obo $(OBO)/go.owl external/cell/cl.owl $(OBO)/uberon/merged.owl $(OBO)/mpath.owl

merged-hp:
	ontology-release-runner --allow-overwrite --outdir hp --repair-cardinality --no-subsets --no-reasoner --asserted --re-mireot hp.obo -b hp/hp-equivalence-axioms.obo $(CORE) $(OBO)/fma.owl $(OBO)/uberon/bridge/uberon-bridge-to-fma.owl

merged-mp:
	ontology-release-runner --allow-overwrite --outdir mp --repair-cardinality --no-subsets --no-reasoner --asserted --re-mireot mp.obo -b mp/mp-equivalence-axioms.obo $(CORE) $(OBO)/ma.owl $(OBO)/uberon/bridge/uberon-bridge-to-ma.owl

build-mp: mp/mp-equivalence-axioms-subq-ubr.owl mp.owl
	ontology-release-runner --catalog-xml $(CATALOG)  --ignoreLock --skip-release-folder --outdir main --simple --allow-overwrite --reasoner elk mp.owl -b $<  $(OBO)/nbo.owl $(OBO)/pato.owl $(OBO)/go.owl $(OBO)/uberon.owl $(OBO)/chebi.owl 

merged-vt:
	ontology-release-runner --catalog-xml $(CATALOG) --allow-overwrite --outdir vt/build  --no-subsets --reasoner elk --simple --asserted --re-mireot vt/vt.obo -b vt/vt-equivalence-axioms.obo $(CORE)


# ----------------------------------------
# APPLICATION ONTOLOGIES AND ANNOTATIONS
# ----------------------------------------

# here we merge an uberon module with phenotype modules
#  the uberon module currently makes use of: develops_from,part_of,continuous_with,capable_of

MP_HP_U = external/uberon/uberon-with-isa-for-FMA-MA-ZFA.owl pato.owl mp/mp-ext-merged.owl hp/hp-ext-merged.owl hp-mp/mp_hp-align-equiv.owl external/behavior-ontology/behavior.owl
# too large for svn?
hp-mp/mp-hp-ext-merged-uberon.owl: $(MP_HP_U)
	owltools $(patsubst %,file:%,$(MP_HP_U)) --merge-support-ontologies -o file://`pwd`/$@

MP_HP_U_PSM = external/uberon/uberon-with-isa-for-FMA-MA-ZFA.owl pato.owl mp/mp-ext-merged-psm.owl hp/hp-ext-merged-psm.owl hp-mp/mp_hp-align-equiv.owl
hp-mp/mp-hp-ext-merged-uberon-psm.owl: $(MP_HP_U_PSM)
	owltools $(patsubst %,file:%,$(MP_HP_U_PSM)) --merge-support-ontologies -o file://`pwd`/$@

phenotype-annotations.owl: external/annotations/MGI/genotype_phenotype.owl external/annotations/Human/disorder_phenotype.owl
	owltools --create-ontology 'http://purl.obolibrary.org/obo/phenotype.owl' file:external/annotations/MGI/genotype_phenotype.owl file:external/annotations/Human/disorder_phenotype.owl --merge-support-ontologies -o file://`pwd`/$@

# ----------------------------------------
# TESTS
# ----------------------------------------
%-names.obo: %.obo
	obo-filter-tags.pl  -t id -t name  $< > $@

# ----------------------------------------
# DEFINITION PATTERNS
# ----------------------------------------
hp-patterns.txt: hp/hp-equivalence-axioms.obo
	blip-findall -r HP -consult util/pattern.pro -i $< pattern_xs/2 -no_pred -label | perl -npe 's/,HP:/\t/g' | sort -u > $@

# ----------------------------------------
# ANALYSIS
# ----------------------------------------
all-by-all.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools file:phenotype-annotations.owl --merge file:$< --sim-method MultiSimilarity  --sim -p 'has phenotype' -a $(OBO)/NCBITaxon_10090 --vs $(OBO)/NCBITaxon_9606 --save-sim file://`pwd`/out.owl >& $@

all-ic.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools file:phenotype-annotations.owl --merge file:$< --all-class-ic > $@

all-closure.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools  file:phenotype-annotations.owl --merge file:$< --monitor-memory --save-closure $@

