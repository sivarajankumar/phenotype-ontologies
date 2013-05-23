OBO=http://purl.obolibrary.org/obo

# get vt.obo from bioportal
vt.owl: vt.obo
	obolib-obo2owl $< -o $@

build: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir main --no-subsets --skip-format owx vt.obo $< $(OBO)/pato.owl $(OBO)/uberon.owl

quick-check: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir test --no-subsets --skip-format owx vt.obo $<


