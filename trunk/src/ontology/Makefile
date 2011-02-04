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

mp.obo:
	wget ftp://ftp.informatics.jax.org/pub/reports/MPheno_OBO.ontology -O $@

hp.obo:
	cp external/hpo/human-phenotype-ontology.obo $@

# todo - syntactic check
mp/mp-equivalence-axioms.obo: edit/mp-equivalence-axioms.obo
	cp $< $@

# todo - syntactic check
hp/hp-equivalence-axioms.obo: external/hpo/human-phenotype-ontology_xp.obo
	cp $< $@

%.owl: %.obo
	obolib-obo2owl --allow-dangling --default-ontology $*  $< -o file://`pwd`/$@

hp/hp-ext-merged.owl: hp.owl hp/hp-equivalence-axioms.owl
	owltools file:$< --merge file:hp/hp-equivalence-axioms.owl -o file://`pwd`/$@

$(ONT)/%-ext-merged.owl: %.owl $(ONT)/%-equivalence-axioms.owl
	owltools file:$< --merge file:$(ONT)/$*-equivalence-axioms.owl -o file://`pwd`/$@

# merge in the rewritten axioms
$(ONT)/%-ext-merged-tr.owl: %.owl $(ONT)/%-equivalence-axioms-tr.owl
	owltools --create-ontology $(OBO)/$@ --merge file:$< --merge file:$(ONT)/$*-equivalence-axioms-tr.owl -o file://`pwd`/$@

# apply POPL transform to rewrite expressions in axiom
%-tr.owl: %.owl
	thea --popl-file std_to_pheno.popl $< --to owl > $@

# ----------------------------------------
# APPLICATION ONTOLOGIES AND ANNOTATIONS
# ----------------------------------------

MP_HP_U = external/uberon/uberon-with-isa-for-FMA-MA-ZFA.owl quality.owl mp/mp-ext-merged.owl hp/hp-ext-merged.owl hp-mp/mp_hp-align-equiv.owl
mp-hp-ext-merged-uberon.owl: $(MP_HP_U)
	owltools $(patsubst %,file:%,$(MP_HP_U)) --merge-support-ontologies -o file://`pwd`/$@

phenotype-annotations.owl: external/annotations/MGI/genotype_phenotype.owl external/annotations/Human/disorder_phenotype.owl
	owltools --create-ontology 'http://purl.obolibrary.org/obo/phenotype.owl' file:external/annotations/MGI/genotype_phenotype.owl file:external/annotations/Human/disorder_phenotype.owl --merge-support-ontologies -o file://`pwd`/$@

# ----------------------------------------
# ANALYSIS
# ----------------------------------------
all-by-all.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools file:phenotype-annotations.owl --merge file:$< --sim-method MultiSimilarity  --sim -p 'has phenotype' -a $(OBO)/NCBITaxon_10090 --vs $(OBO)/NCBITaxon_9606 --save-sim file://`pwd`/out.owl >& $@

all-ic.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools file:phenotype-annotations.owl --merge file:$< --all-class-ic > $@

all-closure.txt: mp-hp-ext-merged-uberon.owl phenotype-annotations.owl
	owltools  file:phenotype-annotations.owl --merge file:$< --monitor-memory --save-closure $@
