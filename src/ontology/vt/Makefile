OBO=http://purl.obolibrary.org/obo

build: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir main --no-subsets --skip-format owx vt.obo $< $(OBO)/pato.owl $(OBO)/uberon.owl $(OBO)/cl-basic.owl

quick-check: vt-equivalence-axioms.obo
	ontology-release-runner --allow-overwrite --reasoner elk --outdir test --no-subsets --skip-format owx vt.obo $<


