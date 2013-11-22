ONT = ../src/ontology/
DATA = ../data
MM = $(DATA)/Mus_musculus
MM_G2P = $(MM)/Mm-gene-to-phenotype-BF.txt
MM_G2L = $(MM)/Mm-gene-labels.txt

ROOT = $(ONT)/mammal.owl
TRIGGERS = $(ONT)/mp.owl $(ONT)/hp.owl $(MM_G2P)

USECAT = --catalog-xml $(ONT)/catalog-v001.xml 

all: caches

all.owl:  $(ROOT) $(TRIGGERS)
	owltools $(USECAT) $< --merge-imports-closure --load-instances $(MM_G2P) --load-labels $(MM_G2L) --merge-support-ontologies -o $@

caches: owlsim.cache ic-cache.owl


owlsim.cache: all.owl
	owltools $< --fsim-compare-atts  -p owlsim-cache.properties -o $@.full --sim-save-lcs-cache $@

ic-cache.owl: all.owl
	owltools $< --no-debug --sim-save-ic-cache $@

