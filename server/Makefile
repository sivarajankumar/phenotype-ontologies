ONT = ../src/ontology/
DATA = ../data

MM = $(DATA)/Mus_musculus
MM_G2P = $(MM)/Mm-gene-to-phenotype-BF.txt
MM_G2L = $(MM)/Mm-gene-labels.txt

DR = $(DATA)/Danio_rerio
DR_G2P = $(DR)/Dr-gene-to-phenotype-BF.txt
DR_G2L = $(DR)/Dr-gene-labels.txt

# TODO: genes
DM = $(DATA)/Drosophila_melanogaster
DM_G2P = $(DM)/Dm-genotype-phenotype-NOFILTER.txt
DM_G2L = $(DM)/Dm-genotype-labels.txt

HS = $(DATA)/Homo_sapiens
HS_D2P = $(HS)/Hs-disease-to-phenotype-O.txt
HS_D2L = $(HS)/Hs-disease-labels.txt

##ROOT = $(ONT)/mammal.owl
ROOT = $(ONT)/vertebrate.owl
TRIGGERS = $(ONT)/mp.owl $(ONT)/hp.owl $(ONT)/zp.owl $(MM_G2P) $(DR_G2P) $(HS_D2P)

USECAT = --catalog-xml $(ONT)/catalog-v001.xml 

OWLTOOLS = OWLTOOLS_MEMORY=12G owltools

all: caches
caches: owlsim.cache ic-cache.owl

all-%: %-owlsim.cache %-ic-cache.owl

all.owl:  $(ROOT) $(TRIGGERS)
	$(OWLTOOLS) $(USECAT) $< --merge-imports-closure --load-instances $(MM_G2P) --load-labels $(MM_G2L) --load-instances $(DR_G2P) --load-labels $(DR_G2L) --load-instances $(HS_D2P) --load-labels $(HS_D2L) --merge-support-ontologies -o $@

Mus_musculus-all.owl:  $(ONT)/mp/mp-importer.owl $(TRIGGERS)
	owltools $(USECAT) $< --merge-imports-closure --load-instances $(MM_G2P) --load-labels $(MM_G2L) --merge-support-ontologies -o $@
Homo_sapiens-all.owl:  $(ONT)/hp/hp-importer.owl $(TRIGGERS)
	owltools $(USECAT) $< --merge-imports-closure --load-instances $(HS_D2P) --load-labels $(HS_D2L) --merge-support-ontologies -o $@
Drosophila_melanogaster-all.owl:  $(ONT)/dpo/dpo-importer.owl $(TRIGGERS)
	owltools $(USECAT) $< --merge-imports-closure --load-instances $(DM_G2P) --load-labels $(DM_G2L) --merge-support-ontologies --remove-axioms -t DisjointClasses -o $@


#owlsim.cache: all.owl
#	$(OWLTOOLS) $< --sim-save-ic-cache ic-cache-pre.owl --sim-save-lcs-cache -m 3.0 $@ --sim-save-ic-cache ic-cache-redundant.owl
owlsim.cache: all.owl
	$(OWLTOOLS)  $< --sim-save-lcs-cache --debug-class MP:0000001 -m 3.0 $@ --sim-save-state state1.txt --sim-save-ic-cache ic-cache-redundant.owl --sim-save-state state2.txt

ic-cache.owl: all.owl
	$(OWLTOOLS) $< --no-debug --sim-save-ic-cache $@


%-owlsim.cache: %-all.owl
	$(OWLTOOLS) $< --sim-save-lcs-cache -m 2.0 $@ --sim-save-ic-cache $*-ic-cache-redundant.owl

%-ic-cache.owl: %-all.owl
	$(OWLTOOLS) $< --no-debug --sim-save-ic-cache $@

