
# Background

Existing phenotype ontologies are arranged as simple classification
hierarchies, with broader phenotype terms subsuming more specific
ones. One notable feature of this design is that the *classification*
of the phenotype is conflated with *causal information* connecting
phenotypes. This is not a problem when using a phenotype ontology for
search or simple grouping, but this can be a problem when we try and
use gene-phenotype associations in a more sophisticated way.

For example, this subset of the Mammalian Phenotype (MP) ontology:

```
  is_a MP:0005379 ! endocrine/exocrine gland phenotype
   is_a MP:0002163 ! abnormal gland morphology
    is_a MP:0000681 ! abnormal thyroid gland morphology
     is_a MP:0002951 ! small thyroid gland
     is_a MP:0003421 ! abnormal thyroid gland development ***
     is_a MP:0004663 ! athyroidism
     is_a MP:0004696 ! abnormal thyroid follicle morphology
     is_a MP:0004698 ! abnormal thyroid parafollicular C-cell morphology
     is_a MP:0005355 ! enlarged thyroid gland
     is_a MP:0013162 ! abnormal thyroid gland isthmus morphology
     is_a MP:0013233 ! ectopic thyroid gland
```

The highlighted term is a developmental (process) term alongside and
under a morphology term. The relationship between the development of
the gland and its morphology is one of causation, rather than subtype.

For this subset of the Human Phenotype (HP) ontology:

```
   is_a HP:0001939 ! Abnormality of metabolism/homeostasis
    is_a HP:0010932 ! Abnormality of nucleobase metabolism
     is_a HP:0004352 ! Abnormality of purine metabolism
      is_a HP:0004368 ! Increased purine levels ***
      is_a HP:0004369 ! Decreased purine levels ***
      is_a HP:0010933 ! Abnormality of xanthine metabolism
      is_a HP:0011814 ! Increased urinary hypoxanthine
```

The highlighted term is a amount (object) term alongside and under a
process term. The relationship between purine levels and purine
metabolism should be is-caused-by.

Furthermore, this structure doesn't give us an easy means of answering
the question as to the effects of mutating genes known to be involved
in the GO biological process 'purine biosynthesis' (loss of this gene
may lead to decreased levels, a gain of function mutation may lead to
increased levels).

Furthermore, it can be argued that the generic structure of placing "X
level" phenotypes under "X metabolism" is incorrect, because changes
in the *levels* of a substance are not necessarily due to changes in
metabolism - the changes may be due to abnormalities in import of
export of the substance.

## Related Work

CPO

## CPN

We have created a phenotype ontology CPN that separates the
classification of a phenotype from its causal effect. Specifically,
phenotypes that are due to *processual* changes form a disjoint
hierarchy from those that are due to changes in attributes of
*physical objects* such as chemical concentrations, cellular and organ
morphology. The ontology is constructed by an automatic process using
knowledge encoded in ontologies such as GO and UBERON.

CPN is species-neutral, and is intended to be used as a module by
existing phenotype and trait ontologies. It can easily slot into
existing OWL-based phenotype development workflows; whilst CPN itself
forms disjoint hierarchies, end-product ontologies automatically
collapse these distinctions into a simple and familiar subsumption
hierarchy.

# Results

## Construction of Causal Phenotype Network Ontology

The construction process makes use of knowledge encoded as logical
axioms in existing ontologies.

For example, go-plus entails an inter-ontology axiom

```
'thyroid gland morphogenesis' SubClassOf results_in_morphogenesis_of some 'thyroid gland'
```

We treat this as a shortcut relation that can be expanded to:

```
'thyroid gland morphogenesis' SubClassOf regulates some (morphology and inheres_in some 'thyroid gland')
```

We find all axioms of this form and generate two phenotype terms (one
processual, one object quality) and create a causation relation
between them:

```
Class: 'thyroid gland morphology'
 EquivalentTo: morphology and inheres_in some 'thyroid gland'

Class: 'thyroid gland morphogenesis phenotype'
 EquivalentTo: quality and inheres_in some 'thyroid gland morphogenesis'
 SubClassOf: causes some 'thyroid gland morphology'
```

Whilst the two classes seem like close and even indistinguishable
concepts, it is useful to draw a distinction, because changes in
morphology may be due to other facts that changes in morphogenesis.

We can also apply the same pattern to developmental absence: if the
development of a structure does not exist, then the structure can not
exist (but non-existence of the structure could be due to other
reasons).

When applying the same strategy to developmental processes such as
growth, we can introduce directional qualities. For example, if a
growth process is reduced in rate or efficiency, then the resulting
structure will be (all other things equal) smaller.

The patterns also work on a cellular and biochemical level. For
example: if the rate of disassembly of X is *decreased*, then this
will *increase* the levels of X.

 * "X assembly" phenotype -> X levels
    * increased "X assembly" -> increased X levels
    * decreased "X assembly" -> decreased X levels
 * "X disassembly" phenotype -> X levels
    * increased "X assembly" -> decreased X levels
    * decreased "X assembly" -> increased X levels

## Classification and bridging with existing phenotype ontologies

Automated reasoning over CPN yields subsumption hierarchies that are
heavily stratified - for example, process phenotypes will classify
separate from object phenotypes. Morphology hierarchies will classify
separately from volume hierarchies and so on.

For end-product phenotype ontologies, the current paradigm is to
provide mixed hierarchies in which conceptually close terms group
together. In order to support this conflation, OWL definitions for
existing phenotype ontologies include a 'bundling' property (which we
call 'has' here for simplicit). Without this property contradictions
arise in reasoning.

In order to recapitulate existing conflationary structures, we include
a property chain:

```
has o causes -> cause
```

This has the effect of classifying $has 'thyroid gland morphogenesis'$
under $has 'thyroid gland morphology'$, recapitulating existing
structures.

TODO: abnormal

## Propagation of genes

Of of the goals of this method is to effectively unify gene
sets. Currently it is difficult to leverage gene sets deriving from
phenotype curation in GO annotation, and vice versa. This framework
provides a method for inferring the phenotype annotation for a GO
annotation.

For example, a gene that is known to be involved in 'purine
catabolism' (through some experimental assay) can be propagated to the
corresponding phenotype term; e.g. we can infer that mutations in the
gene can lead to 'abnormal purine catabolism' (of course, this may not
always be the case, e.g. if the gene is rescued by a functionally
similar or identical gene in the genome). Furthermore, we can infer
that mutations in the gene may also lead to 'abnormal purine levels'.



# Methods
