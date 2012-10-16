omia-genes.txt:
	wget 'http://omia.angis.org.au/curate/causal_mutations/?format=gene_table' -O $@
.PRECIOUS: omia-genes.txt

omia-phene-id-name.txt: omia-genes.txt
	cut -f3,6 $< | perl -npe 's/^/OMIA:/' > $@

omia-s1.obo: omia-phene-id-name.txt
	tbl2obo.pl $< | perl -npe 's/name: (\w)/"name: ".lc($$1)/e' > $@

