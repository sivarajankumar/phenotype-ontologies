# Introduction #

This page describes the generation of the zebrafish phenotype ontology

# Details #

The ZP differs considerably from [HP](HP.md), [MP](MP.md) and others. ZFIN do not annotate with a pre-composed phenotype ontology - all annotations compose phenotypes on-the-fly using a combination of PATO, ZFA, GO and other ontologies.

We use these combinations to construct ZP on the fly, by naming each distinct combination, assigning it an ID, and placing it in the hierarchy.

The process is described here:

**Sebastian Köhler, Sandra C Doelken, Barbara J Ruef, Sebastian Bauer, Nicole Washington, Monte Westerfield, George Gkoutos, Paul Schofield, Damian Smedley, Suzanna E Lewis, Peter N Robinson, Christopher J Mungall (2013) [Construction and accessibility of a cross-species phenotype ontology along with gene annotations for biomedical research](http://f1000research.com/articles/2-30/v1) F1000Research**

The OWL formalism for ZFIN annotations is described here:

**[Mapping ZFIN phenotypes to OWL](https://docs.google.com/document/d/1Vbokc9aFHR4awNE6DrrLtgpE6axeTS4VEfxqDHsWyPQ/edit#)**

The java implementation is here:

**https://code.google.com/p/bio-ontology-zp/**

# OWL Axiomatization #

The OWL axioms for ZP are in the [src/ontology/zp](http://phenotype-ontologies.googlecode.com/svn/trunk/src/ontology/zp/) directory on this site.

Because ZP is built "pre-axiomatized", the equivalence axioms are already present in zp.owl

  * http://purl.obolibrary.org/obo/zp.owl - ZP pre-reasoned auto-ontology
  * http://purl.obolibrary.org/obo/zp/zp-importer.owl - brings in other ontologies
