

align-%.txt: mtb.obo
	blip-findall -consult nlp_filter.pro -u metadata_nlp -u metadata_nlp_disorder_hook -goal index_entity_pair_label_match -i $< -r $* "entity_pair_label_reciprocal_best_intermatch(A,B,S)" -use_tabs -label -no_pred > $@.tmp && grep -v UMLS: $@.tmp |  sort -u > $@
.PRECIOUS: align-%.txt

xref-%-strict.obo: align-%.txt
	grep ^MTB $< | grep true | cut -f1-4 | tbl2obolinks.pl --rel xref > $@
