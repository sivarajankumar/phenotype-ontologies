Phenotype Ontologies
====================

This project contains OWL derived from existing phenotype
ontologies. These ontologies are enhanced by additional axioms[1, 2]
which allow them to be used for reasoning and multi-species data
discovery.

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
* ONT/ONT-equivalence-axioms.owl -- external logical definitions copied from source (e.g. mp_xp). See [1].
* ONT/ONT-ext-merged.owl -- merge of ONT and its equivalence axioms into one ontology
* ONT/ONT-ext-merged-tr.owl -- merged ontology transformed to the more expressive form documented in [2].

For the final step, the POPL transform std_to_pheno.popl is used. See
this file for details.

For more details on POPL see http://blipkit.wordpress.com/popl

Ontology URLs
-------------

The contents of this project should mirror the
http://purl.obolibrary.org/obo/ directory structure. See [4] for more
details. The structure should be as follows:

* http://purl.obolibrary.org/obo/hp/hp-ext-merged-tr.owl
* http://purl.obolibrary.org/obo/mp/mp-ext-merged-tr.owl
* http://purl.obolibrary.org/obo/wbphenotype/wbphenotype-ext-merged-tr.owl
* http://purl.obolibrary.org/obo/apo/apo-ext-merged-tr.owl
* http://purl.obolibrary.org/obo/to/to-ext-merged-tr.owl

You can use a catalog.xml to map these URLs

Full Details
------------

Phenotype ontologies are typically managed as simple hierarchies. We
demonstrated how providing equivalence axioms for these can allow us
to use reasoners to automate ontology construction[1] and guide
cross-species analyses [5]. We adopted a simple representational
pattern, the EQ model[6], that can easily be represented in obo format
and model organism databases. For example:

EQ Syntax: E=bone, Q=increased density

OBO Format:

    [Term]
    id: MP:0000062 ! increased bone density
    intersection_of: PATO:0001788 ! increased density
    intersection_of: inheres_in MA:0001459 ! bone

    [Typedef]
    id: inheres_in
    name: inheres_in
    xref: BFO:0000052

These logical definitions are maintained as separate bridge
ontologies in obo format.

OBO Format can be treated as just another notation for OWL. The above
can also be written in a more conventional OWL notation:

    Class: MP_0000062
    EquivalentTo: PATO_0001788 and BFO_0000052 some MA_0001459

The core ontology and the external bridging ontology are collected
here in this structure:

    ONT.owl
    ONT/
        ONT-equivalence-axioms.owl
        ONT-ext-merged.owl

The ext-merged file is an ontology with both the core hierarchy and
metadata merged with equivalence axioms.

The pipeline here uses the standard obo2owl parser
(http://code.google.com/p/oboformat/), which is the only one to
correctly treat all axioms according to the obof1.4 standard.

This simple representation is sufficient for many reasoning and data
integration tasks. For example, a reasoner can infer that 'increased
forelimb bone density' is subsumed by 'increased bone density'.

However, this simplified pattern can lead to false negatives in
reasoning for complex phenotypes. See
http://www.obofoundry.org/wiki/index.php/PATO:Complex_qualities

Hoehndorf et al proposed a pattern similar to the above, such that
this phenotype would be represented as:

    'phenotype of' some 
      ('has part' some bone and 'has quality' some 
        'increased density')

We provide translations of the simple EQ to this form using POPL
transforms. These are ontology rewrite rules that serve as executable
specifications. See the file std_to_pheno.popl in this directory, and
also [7].


For example, at the top of the .popl file we have the following core
translation:

    Q and 'inheres in' some E ===> 
      'phenotype of' some ('has part' some E and 'bearer of' some Q).

The basic POPL syntax is similar to manchester syntax, with `LHS ===>
RHS` production/rewrite rules. All variables have a leading upper
case. labels with spaces must be enclosed in quotes (the syntax is
actually prolog, with custom infix rules).

There are in fact many POPL rules to account for the variety of
patterns used (see
http://www.obofoundry.org/wiki/index.php/PATO:XP_Rules for a full list
of patterns).

The full set of OWL files generated and made available here are:

    ONT.owl
    ONT/
        ONT-equivalence-axioms.owl
        ONT-equivalence-axioms-tr.owl
        ONT-ext-merged.owl
        ONT-ext-merged-tr.owl

The files suffixed "-tr.owl" have the popl translations applied.

Note that in the tr files there are still some simple EQ patterns that
have not been translated.

TODO
----

* ensure patterns in popl file are complete
* handle imports/merges with external ontologies
* add disjointness axioms to PATO and other ontologies

Requirements and availability
-----------------------------

Standard OWL tools are all that is required to use the OWL files generated here.

To run the pipeline yourself, you will need two applications

 * OWL2LS  -- java OWLAPI wrapper code - http://wiki.geneontology.org/index.php/OWLTools
 * POPL -- distributed as part of Thea -- http://semanticweb.gr/thea


References
----------

* [1] Mungall et al http://genomebiology.com/2010/11/1/R2/
* [2] Hoehndorf et al http://bioinformatics.oxfordjournals.org/content/26/24/3112.full
* [3] http://berkeleybop.org/~cjm/obo2owl/obo-syntax.html
* [4] http://obo-foundry.org/id-policy.shtml
* [5] http://www.plosbiology.org/article/info%3Adoi%2F10.1371%2Fjournal.pbio.1000247
* [6] Representing Phenotypes in OWL http://www.webont.org/owled/2007/PapersPDF/paper_40.pdf
* [7] http://blipkit.wordpress.com/popl
