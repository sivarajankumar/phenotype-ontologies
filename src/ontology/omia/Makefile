omia-genes.txt:
	wget 'http://omia.angis.org.au/curate/causal_mutations/?format=gene_table' -O $@
.PRECIOUS: omia-genes.txt

omia-phene-id-name.txt: omia-genes.txt
	cut -f3,6 $< | perl -npe 's/^/OMIA:/' > $@

omia-s1.obo: omia-phene-id-name.txt
	tbl2obo.pl $< | perl -npe 's/name: (\w)/"name: ".lc($$1)/e' > $@

omim-rels.obo: omia.xml
	./parse-omim-xrefs.pl $< | tbl2obolinks.pl --rel similar_to > $@

align-%.txt: omia.obo
	blip-findall -consult nlp_filter.pro -u metadata_nlp -u metadata_nlp_disorder_hook -goal index_entity_pair_label_match -i $< -r $* "entity_pair_label_reciprocal_best_intermatch(A,B,S)" -use_tabs -label -no_pred > $@.tmp && grep -v UMLS: $@.tmp |  sort -u > $@
.PRECIOUS: align-%.txt

