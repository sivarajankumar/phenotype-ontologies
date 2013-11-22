ONT = ../src/ontology/
DATA = ../data
MM = $(DATA)/Mus_musculus

owlsim.cache: upheno.obo
	owltools $(USECAT)  upheno.obo --load-instances $(MM)/Mm-gene-to-phenotype-BF.txt --sim-compare-atts  -p config/owlsim-cache.properties -o $@.full --sim-save-lcs-cache $@
ic-cache.owl: upheno.obo
	owltools $(USECAT)  upheno.obo --load-instances $(MM)/Mm-gene-to-phenotype-BF.txt --no-debug --sim-save-ic-cache $@

