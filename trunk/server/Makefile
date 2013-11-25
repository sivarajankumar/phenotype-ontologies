ONT = ../src/ontology/
DATA = ../data

MM = $(DATA)/Mus_musculus
MM_G2P = $(MM)/Mm-gene-to-phenotype-BF.txt
MM_G2L = $(MM)/Mm-gene-labels.txt

DR = $(DATA)/Danio_rerio
DR_G2P = $(DR)/Dr-gene-to-phenotype-BF.txt
DR_G2L = $(DR)/Dr-gene-labels.txt

##ROOT = $(ONT)/mammal.owl
ROOT = $(ONT)/vertebrate.owl
TRIGGERS = $(ONT)/mp.owl $(ONT)/hp.owl $(ONT)/zp.owl $(MM_G2P) $(DR_G2P)

USECAT = --catalog-xml $(ONT)/catalog-v001.xml 

all: caches

all.owl:  $(ROOT) $(TRIGGERS)
	owltools $(USECAT) $< --merge-imports-closure --load-instances $(MM_G2P) --load-labels $(MM_G2L) --load-instances $(DR_G2P) --load-labels $(DR_G2L) --merge-support-ontologies -o $@

caches: owlsim.cache ic-cache.owl


owlsim.cache: all.owl
	OWLTOOLS_MEMOTY=12G owltools $< --sim-save-lcs-cache -m 3.0 $@

ic-cache.owl: all.owl
	owltools $< --no-debug --sim-save-ic-cache $@

