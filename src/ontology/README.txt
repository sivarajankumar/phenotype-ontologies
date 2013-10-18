Phenotype Ontologies
====================

This project contains OWL derived from existing phenotype
ontologies. These ontologies are enhanced by additional axioms[1, 2]
which allow them to be used for reasoning and multi-species data
discovery.

See the google code repo page for more details
https://code.google.com/p/phenotype-ontologies/

Ontologies
----------

 * http://purl.obolibrary.org/obo/hp.owl -- the Human Phenotype Ontology
 * http://purl.obolibrary.org/obo/mp.owl -- the Mammalian Phenotype Ontology
 * http://purl.obolibrary.org/obo/wbphenotype.owl -- the Worm Phenotype Ontology
 * http://purl.obolibrary.org/obo/apo.owl -- the Ascomycetes Phenotype Ontology Ontology
 * http://purl.obolibrary.org/obo/to.owl -- the Plant Trait Ontology Ontology

Pipeline
--------

The pipeline is specified by the Makefile in this directory.

* ONT.obo -- this is copied from source. Should be identical to http://purl.obolibrary.org/obo/ONT.obo
* ONT.owl -- uses new [3] obo2owl translation. Should be identical to http://purl.obolibrary.org/obo/ONT.owl
* ONT/ONT-equivalence-axioms-subq-ubr.owl -- external logical definitions with anatomy mapped to uberon where appropriate
* ONT/ONT-importer.owl -- imports axioms, core ontology plus subsets of OBO ontologies


Ontology URLs
-------------

The contents of this project should mirror the
http://purl.obolibrary.org/obo/ directory structure. See [4] for more
details. 

References
----------

* [1] Mungall et al http://genomebiology.com/2010/11/1/R2/
* [2] Hoehndorf et al http://bioinformatics.oxfordjournals.org/content/26/24/3112.full
* [3] http://oboformat.org
* [4] http://obo-foundry.org/id-policy.shtml
* [5] http://www.plosbiology.org/article/info%3Adoi%2F10.1371%2Fjournal.pbio.1000247
* [6] Representing Phenotypes in OWL http://www.webont.org/owled/2007/PapersPDF/paper_40.pdf


