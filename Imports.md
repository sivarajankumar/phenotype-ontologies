# Imported ontologies

# Introduction #

Imports directory:

  * http://purl.obolibrary.org/obo/upheno/imports/

Currently the imports includes:

  * imports/chebi\_import.owl
  * imports/doid\_import.owl
  * imports/go\_import.owl
  * imports/mpath\_import.owl
  * imports/pato\_import.owl
  * imports/pr\_import.owl
  * imports/uberon\_import.owl
  * imports/wbbt\_import.owl

# Anatomy #

To avoid multiple duplicate classes for heart, lung, skin etc we map all classes to [Uberon](Uberon.md) where this is applicable. For more divergent species such as fly and C elegans we use the appropriate species-specific ontology.

Currently there are a small number of highly specific classes in FMA that are being used and have no corresponding class in Uberon

# Methods #

We use the OWLAPI SyntacticLocalityModularityExtractor, via [OWLTools](OWLTools.md). See the http://purl.obolibrary.org/obo/upheno/Makefile for details