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

all: $(patsubst %,%-all,$(ONTS)) all_imports mammal-reasoned.owl vertebrate-reasoned.owl metazoa-reasoned.owl upheno.owl
all_obo : $(patsubst %,%.obo,$(ONTS))
all_xp_obo : $(patsubst %,%-equivalence-axioms.obo,$(ONTS))
all_owl : $(patsubst %,%.owl,$(ONTS))

%-all: %.obo %.owl
	make $*/$*-equivalence-axioms-subq-ubr.owl ONT=$*


# OVERRIDES
mp/mp-equivalence-axioms-subq-ubr.owl: mp/mp-equivalence-axioms-edit.owl
	owltools $(USECAT) $< --remove-imports-declarations -o $@

# ----------------------------------------
# PARSING, TRANSLATION AND REWRITING
# ----------------------------------------

refresh:
	yes p | svn update && mv mp.obo mp.obo.old && make mp.obo && mv hp.obo hp.obo.old && make hp.obo

mp.obo:
	wget $(MP_URL) -O $@
mp.owl: mp.obo
	owltools $< -o $@



hp.obo: external/hpo/human-phenotype-ontology.obo
	echo remember to update external/hpo && \
	cp $< $@
hp.owl: hp.obo
	owltools $< -o $@

zp.owl:
	wget http://compbio.charite.de/hudson/job/hpo.ontology.uberpheno/lastStableBuild/artifact/data/dataForUberpheno/not_subsettable_ontologies/zp.owl
zp.obo: zp.owl
	owltools $< -o -f obo --no-check $@

zp/zp-equivalence-axioms-subq.owl: zp.owl
	cp $< $@

fypo.owl:
	wget $(OBO)/fypo.owl

wbphenotype.owl:
	wget $(OBO)/wbphenotype.owl
wbphenotype.obo: wbphenotype.owl
	owltools $< -o -f obo --no-check $@

upheno.obo:
	wget http://compbio.charite.de/hudson/job/hpo.ontology.uberpheno/lastStableBuild/artifact/data/uberphenoOut/crossSpeciesPheno.obo -O $@
upheno.owl: upheno.obo
	owltools $< -o $@

pato.obo:
	cp $(HOME)/cvs/obo/ontology/phenotype/quality.obo $@
pato.owl: pato.obo
	owltools $< -o $@

# todo - syntactic check
#mp/mp-equivalence-axioms.obo: edit/mp-equivalence-axioms.obo
#	cp $< $@

# todo - syntactic check
hp/hp-equivalence-axioms.obo: external/hpo/human-phenotype-ontology_xp.obo
	cp $< $@

#mp/%.owl: mp/%.obo
#	owltools $< -o $@
#hp/%.owl: hp/%.obo
#	owltools $< -o $@

#%.owl: %.obo
#	owltools  $< -o file://`pwd`/$@
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
%-merged.owl: %-importer.owl mp/mp-equivalence-axioms-subq-ubr.owl hp/hp-equivalence-axioms-subq-ubr.owl mp.owl hp.owl
	owltools --use-catalog $< --merge-import-closure --ni graph-traversal-metadata.owl --merge-support-ontologies   --merge-equivalent-classes -f $(OBO)/MP_ -t $(OBO)/HP_ --merge-equivalent-classes -f $(OBO)/FMA_ -f $(OBO)/EMAPA_ -f $(OBO)/MA_ -t $(OBO)/UBERON_ --merge-equivalent-classes -f $(OBO)/FMA_ -t $(OBO)/CL_ -t $(OBO)/UBERON_ -o file://`pwd`/$@

# ----------------------------------------
# Subqualities model
# ----------------------------------------
#
# Phenotype as a bag of qualities
#
# Here we transform 'Ph = Q and inheres_in some E' ==> 'Ph = has_part some (Q and inheres_in some E)'
# (or more generally, any such equivalence axiom)
# this better accounts for combined phenotypes, especially in HPO, and the fact that the PATO lineage often switches in the same phenotype hierarchy

%-subq.obo: %.obo
	../tools/make-subq-model-from-obo.pl $< > $@
%-subq.owl: %-subq.obo
	owltools $M -o $@

# temp hack:
fixed-behavior_xp.obo: external/behavior-ontology/behavior_xp.obo
	obo-grep.pl --neg -r PATO $< > $@

# ----------------------------------------
# Uberon
# ----------------------------------------

zp/zp-equivalence-axioms-subq-ubr.owl: zp.owl uberon-simple-vertebrate-bridge.obo
	owltools $< uberon-simple-vertebrate-bridge.obo --add-imports-from-supports --merge-equivalent-classes -f ZFA  -t UBERON  --merge-equivalent-classes -f ZFA -t CL -o $@

%-ubr.obo: %.obo uberon-simple-vertebrate-bridge.obo
	owltools $< uberon-simple-vertebrate-bridge.obo --add-imports-from-supports --merge-equivalent-classes -f FMA -f MA -f ZFA  -t UBERON  --merge-equivalent-classes -f FMA -t CL -o -f obo --no-check $@.tmp && grep -v ^import $@.tmp > $@

# mapping to uberon
# TODO: don't hardcode idspace
#%-ubr.obo: %.obo
#	obo-map-ids.pl --use-xref-inverse --xref-idspace MA $(HOME)/cvs/uberon/uberon_edit.obo $< > $@



UBRIDGE = external/uberon/bridge/uberon-bridge-to-

# hack: render taxon-equivalents as plain equivalents
#  NOTE: this could have unintended consequences and lead to unsatisfiable classes (e.g. check mammary gland)
uberon-simple-vertebrate-bridge.obo: 
	../tools/flatten-taxon-equivalents.pl --ont uberon/uberon-simple-vertebrate-bridge $(UBRIDGE)ma.obo $(UBRIDGE)fma.obo $(UBRIDGE)zfa.obo $(UBRIDGE)ehdaa2.obo external/uberon/bridge/cl-bridge-to-fma.obo > $@.tmp && mv $@.tmp  $@

# ----------------------------------------
# Module extraction
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

# Base URI for local subset imports
UPHENO_IMPORTS_BASE_URI = $(OBO)/upheno/imports

# Ontology dependencies. TODO - nbo
IMPORTS = pato uberon chebi pr go doid mpath nbo

# Make this target to regenerate ALL
all_imports: $(patsubst %, imports/%_import.owl,$(IMPORTS))

KEEPRELS = BFO:0000050 BFO:0000051 RO:0002202 immediate_transformation_of

# Create an import module using the OWLAPI module extraction code via OWLTools.
# We use the standard catalog, but rewrite the import to X to be a local mirror of ALL of X.
# After extraction, we further reduce the ontology by creating a "mingraph" (removes all annotations except label) and by 

#imports/wbbt_import.owl: wbphenotype/wbphenotype-equivalence-axioms-subq-ubr.owl mirror/wbbt.owl
#	owltools  $(USECAT) --map-ontology-iri $(UPHENO_IMPORTS_BASE_URI)/wbbt_import.owl mirror/wbbt.owl $< --merge-imports-closure mirror/$*.owl --add-imports-from-support  --extract-module -s $(OBO)/$*.owl -c --remove-axiom-annotations --make-subset-by-properties $(KEEPRELS) --set-ontology-id $(UPHENO_IMPORTS_BASE_URI)/$@ -o file://`pwd`/$@

imports/%_import.owl: imports/upheno-preimporter.owl mirror/%.owl mp/mp-equivalence-axioms.obo hp/hp-equivalence-axioms.obo
	owltools  $(USECAT) --map-ontology-iri $(UPHENO_IMPORTS_BASE_URI)/$*_import.owl mirror/$*.owl $< --merge-imports-closure mirror/$*.owl --add-imports-from-support  --extract-module -s $(OBO)/$*.owl -c --remove-axiom-annotations --make-subset-by-properties $(KEEPRELS) --set-ontology-id $(UPHENO_IMPORTS_BASE_URI)/$@ -o file://`pwd`/$@

# clone remote ontology locally, perfoming some excision of relations and annotations
mirror/%.owl:
	owltools $(OBO)/$*.owl --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/$*.owl -o $@
# this is actually uberon+cl
mirror/uberon.owl:
	owltools $(OBO)/uberon/ext.owl $(OBO)/cl.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/uberon.owl -o $@
mirror/wbbt.owl:
	owltools $(OBO)/uberon/composite-metazoan.owl $(OBO)/cl.owl $(OBO)/wbls.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/wbbt.owl -o $@
###	owltools $(OBO)/uberon/ext.owl --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --set-ontology-id $(OBO)/uberon.owl -o $@
mirror/go.owl:
	owltools $(OBO)/go.owl --merge-support-ontologies --remove-annotation-assertions -l --remove-dangling-annotations --make-subset-by-properties $(KEEPRELS) --extract-mingraph --set-ontology-id $(OBO)/go.owl -o $@
mirror/pr.obo:
	wget $(OBO)/pr.obo -O $@.tmp && obo-grep.pl -r 'id: PR:' $@.tmp > $@
mirror/pr.owl: mirror/pr.obo
	owltools $< --remove-dangling --set-ontology-id $(OBO)/pr.owl -o $@
.PRECIOUS: mirror/%.owl

imports/%_import.obo: imports/%_import.owl
	owltools $< -o -f obo $@

# ----------------------------------------
# Pre-packaged
# ----------------------------------------

main/metazoa.owl: metazoa.owl hp.owl mp.owl
	ontology-release-runner $(USECAT) --allow-overwrite --outdir main --no-subsets --reasoner elk --asserted --simple $<

%-reasoned.owl: %.owl hp.owl mp.owl
	owltools $(USECAT) $<  --merge-imports-closure --use-reasoner elk --assert-inferred-subclass-axioms --removeRedundant --markIsInferred --allowEquivalencies --set-ontology-id $(UPHENO)/$@ -o $@ 

%-reasoned.obo: %-reasoned.owl
	owltools $< -o $@

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
# MAPPING TO GO
# ----------------------------------------

mp/mp-map2go.tbl: mp/mp-equivalence-axioms-subq-ubr.obo
	blip-findall -i $< -i mp.obo -r go_xp -r go_mg -r go_basic -consult util/gomap.pro ph_go_nr/4 -label -use_tabs -no_pred > $@.tmp && mv $@.tmp $@

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


# ----------------------------------------
# obo2owl
# ----------------------------------------
# this comes last (lowest precedence rule)
%.owl: %.obo
	owltools $< -o $@


# ----------------------------------------
# DEPLOY
# ----------------------------------------
ARTEFACTS = 
deploy:
	rsync -avz $(ARTEFACTS) $(DEPLOY_TARGET)/

