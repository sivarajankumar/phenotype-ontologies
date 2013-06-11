OBO=http://purl.obolibrary.org/obo

# get vt.obo from bioportal
vt.owl: vt.obo
	obolib-obo2owl $< -o $@

vt-plus.obo: vt-equivalence-axioms.obo vt.obo
	obo-merge-tags.pl -t intersection_of vt.obo $< > $@.tmp && mv $@.tmp $@
#	owltools vt.obo $< --merge-support-ontologies -o -f obo $@

build: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir main --no-subsets --skip-format owx vt.obo $< $(OBO)/pato.owl $(OBO)/uberon.owl

quick-check: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir test --no-subsets --skip-format owx vt.obo $<


