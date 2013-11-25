OBO = http://purl.obolibrary.org/obo
UPHENO = $(OBO)/upheno
ONT = ../src/ontology/
DATA = ../data
MM = $(DATA)/Mus_musculus
DR = $(DATA)/Danio_rerio
MM_G2P = $(MM)/Mm-gene-to-phenotype-BF.txt
MM_G2L = $(MM)/Mm-gene-labels.txt
DR_G2P = $(DR)/Dr-gene-to-phenotype-BF.txt
DR_G2L = $(DR)/Dr-gene-labels.txt

TRIGGERS = $(ONT)/mp.owl $(ONT)/hp.owl $(MM_G2P)

USECAT = --catalog-xml $(ONT)/catalog-v001.xml 

Dr-gene-to-Dr-gene-sim.out:
	OWLTOOLS_MEMORY=12G owltools  $(USECAT) $(UPHENO)/vertebrate.owl  --load-labels $(DR_G2L) --load-instances $(DR_G2P) --fsim-basic -p sim.properties --set minimumMaxIC 3.0 --set minimumSimJ 0.25 --set compare "ZFIN,ZFIN" -o $@

Mm-gene-to-Mm-gene-sim.out:
	OWLTOOLS_MEMORY=12G owltools  $(USECAT) $(UPHENO)/vertebrate.owl  --load-labels $(MM_G2L) --load-instances $(MM_G2P) --fsim-basic -p sim.properties --set minimumMaxIC 3.0 --set minimumSimJ 0.25 --set compare "MGI,MGI" -o $@

Dr-gene-to-disease-sim.out:
	owltools  $(USECAT) $(UPHENO)/vertebrate.owl  --load-labels $(DR_G2L) --load-instances $(DR_G2P) --fsim-basic -p sim.properties --set minimumMaxIC 3.0 --set minimumSimJ 0.6 --set compare "OMIM,ZFIN" -o $@



