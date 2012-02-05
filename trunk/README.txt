Phenotype Ontologies
====================

This repository aggregates together phenotype ontologies and logical definitions from a variety of different species. See:

    Mungall, C. J., Gkoutos, G. V., Smith, C. L., Haendel, M. A., Lewis, S. E., and Ashburner, M. (2010). 
    Integrating phenotype ontologies across multiple species. 
    Genome Biology 11, R2. 
    Available at: http://dx.doi.org/10.1186/gb-2010-11-1-r2

The directory structure follows the standard one recommended for ontology projects:

  src/
    tools/
    ontology/
  docs/

Most of the useful material is in the ontology directory.

Use of svn:externals
--------------------

We use svn:externals to make working with multiple ontologies
easier. Whilst every ontology can be obtained from the web via it's
standard ontology URI (e.g. http://purl.obolibrary.org/obo/pato.owl),
it is easier to have local file access in a standard place.

The current structure is:

  src/
    ontology/
      external/
        anatomy/
        cell/
        hpo/
        ...

Note that none of the directories below "external" are actually in this repository - they are dynamically linked.

See the README.txt in the externals directory for more details.

Tools directory
---------------

The tools directory contains ad-hoc scripts mostly for performing
translations on the logical definitions files

Ontologies
----------

The directory structure of the ontology directory attempts to mirror the structure of the PURLs. So we have:

  mp.owl
  mp.obo
  mp/
    mp-equivalence-axioms.owl
    mp-equivalence-axioms.obo
  hp/
    hp-equivalence-axioms.owl
    hp-equivalence-axioms.obo
  ...

Note that the primary location of the mp equivalence axioms is here -
however, the hpo svn repo (linked via svn externals) is primary for
the hp logical definitions.

Note that the src/ontology directory contains a catalog-v001.xml. This
is used by Protege 4 and by OWLTools to map ontology global URIs to
locations within the local filesystem.

Makefile
--------

Aggregating information is controlled entirely by the Makefile in the
src/ontology/ directory.

To rebuild a target, just type

  make <TARGET>

You may need owltools installed.


Entity matching
---------------

The hp-mp/ directory contains the results of the automated lexical
entity matching between HPO and MPO. We also manually maintain a file
of disjoints that override the results of the entity matching, where
we know the label match does not translate to a real equivalence.

We also provide an obo and owl file of these matches. We treat these
as equivalence axioms. This is potentially problematic, due to
differences between mouse and human.

Importer ontologies
-------------------

We provide a number of importer ontologies. These contain no axioms of
their own, only owl:import declarations to other ontologies.

For more on the concept of importers, see:

  http://wiki.geneontology.org/index.php/Ontology_extensions

There are two main importers:

 * uberpheno-importer.owl
 * uberpheno-subq-importer.owl

Both import human and mouse phenotype ontologies, together with equivalence axioms and connected ontologies.

The difference is that the first equates phenotypes with qualities
inheres in entities, the second equates phenotypes with *bundles* of
qualities inhering in entities

Different representations of phenotypes
---------------------------------------

The original template of

    ?Phenotype EquivalentTo ?Quality and inheres_in some ?Entity

does not work well for HPO because it has many phenotypes that are *bundles* of qualities.

We therefore explore an alternative representation:

    ?Phenotype EquivalentTo has_part some (?Quality and inheres_in some ?Entity)

i.e. the phenotype is a collection of qualities.

This allows composite phenotypes. E.g.

    ?Phenotype EquivalentTo has_part some (?Quality1 and inheres_in some ?Entity1)
             and has_part some (?Quality2 and inheres_in some ?Entity2)

This is called the "subquality" model. Files are named "-equivalence-axioms-subq.owl"

