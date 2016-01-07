

# Introduction #

This is background material for discussion amongst OWL modelers concerning the consequences of modeling choices.

Each competency question is stated in terms of an OWL axiom or axioms that should be entailed, given the existence of supporting axioms in external ontologies. This could be translated into a test suite (e.g. using junit) to objectively test a given model.

examples are given in Manchester syntax. We assume prefixes of the form

```
Prefix: MP <http://purl.obolibrary.org/obo/MP_>
```

Together with imports of relevant ontologies.

We take existing phenotype ontologies as a kind of "gold standard" and assume that assertions provided are correct - of course it may be the case that further discussion will lead to some existing patterns being changed. However, this is expected to be unusual.

# Details #

## Core Competency Questions ##

The model **must** be able to derive the desired entailments here.

### CQ: Basic Inheritance using E class hierarchy ###

Example:
```
Class: MP:0000550 ## abnormal forelimb morphology
SubClassOf: MP:0002109 ## abnormal limb morphology
```

### CQ: Basic Inheritance using Q class hierarchy ###

Example:

```
Class: MP:0008153 ## decreased diameter of fibula
SubClassOf: MP:0002187 ## abnormal fibula morphology
```

Supporting axioms:
```
   is_a PATO:0000051 ! morphology
    is_a PATO:0000117 ! size
     is_a PATO:0000587 ! decreased size
      is_a PATO:0000574 ! decreased length
       is_a PATO:0001715 ! decreased diameter
```

And:

```
Class: PATO:0001715 ## decreased diameter
EquivalentTo: PATO:0001334  ## diameter
  and decreased_in_magnitude_relative_to some PATO:0000461 ## normal
```


### CQ: Basic Inheritance using E partonomy ###

Example:
```
Class: MP:0002110 ## abnormal digit morphology
SubClassOf: MP:0000572 ## abnormal autopod morphology
```

Existing approaches:

The approach described in [Mungall et al 2010](http://genomebiology.com/2010/11/1/R2/#sec2) uses a new relation:

```
ObjectPorperty: inheres_in_part_of
SubPropertyChain: inheres_in o part_of
ObjectProperty: inheres_in SubPropertyOf: inheres_in_part_of
```

Then, assuming a basic Q-in-E model, we can write:

```
Class: MP:0002110 ## abnormal digit morphology
EquivalentTo: morphology and inheres_in_part_of some digit and qualifier abnormal
Class: MP:0000572 ## abnormal autopod morphology
EquivalentTo: morphology and inheres_in_part_of some digit and qualifier abnormal
```

To obtain the desired inference (also works if the digit morphology term is restricted to inheres\_in).

Informally, we can think of this in terms of a weaker relation that permits propagation up the E partonomy (and subclass), and a stronger relation that only propagates up subclass.

The main challenge here is deciding which qualities should be propagated when. An abnormal morphology of a nucleus of a muscle cell of digit 1 would satisfy the condition, but this would be an undesirable inference. To recapitulate the desired level of propagation will require a more sophisticated approach.

### CQ: Basic Relational Quality Inference ###

...

## Advanced Competency Questions ##

Some of these may simply be too hard. Others may be controversial w.r.t what the intended meaning of the phenotype is, and what the expected entailment is.

### Absence Reasoning ###

Note that there are many subtleties here. Please read [ModelingOfAbsence](ModelingOfAbsence.md) as an introduction.

#### CQ: Strict Logical Absence ####

Example:
```
Class: 'lack of all hindlimbs'
SubClassOf: 'lack of all femurs'
```

Please refer to [ModelingOfAbsence](ModelingOfAbsence.md) see why the classes used in this example may not be equivalent to any existing MP class or why inferences of this sort may not be useful from a developmental phenotyping context.

We assume background axioms:

```
Class: femur
SubClassOf: part_of some hindlimb
```




#### CQ: Absence of Some ####

Example:
```
Class: MP:0004790 ## absent upper incisors
SubClassOf: MP:0000125 ## absent incisors
```

Refer to [ModelingOfAbsence](ModelingOfAbsence.md) for a discussion on why this is a desirable inference.

#### CQ: absence classifies under morphology ####

Example:
```
Class: MP:0000690 ## absent spleen
SubClassOf: MP:0000689 ## abnormal spleen morphology
```

#### CQ: absence classifies under decreased number ####

Example:
```
Class: MP:0011696 ## absent mast cells
SubClassOf: MP:0000336 ## decreased mast cell number
```


### Developmental Competency Questions ###

To see how MP is currently classified, see this example:

```
        is_a MP:0002100 ! abnormal tooth morphology
         is_a MP:0000116 ! abnormal tooth development *** 
          is_a MP:0000117 ! absent tooth primordium
          is_a MP:0000118 ! arrest of tooth development
          is_a MP:0000128 ! growth retardation of molars
          is_a MP:0002650 ! abnormal ameloblast morphology
          is_a MP:0002817 ! abnormal tooth mineralization
          is_a MP:0005359 ! growth retardation of incisors
          is_a MP:0010747 ! abnormal enamel organ morphology
          is_a MP:0011165 ! abnormal tooth root development
```

We could argue that the pattern should be switched here, and that abnormal tooth morphology generally arises through abnormal tooth development. However, it is of course possible for teeth to attain abnormal morphology post-development.

Some of the justifications for the other relationships in the above example are more subtle.

More input from biologists required to inform competency questions here.

#### CQ: development classifies under morphology ####

See above

```
Class: MP:0000116 ## abnormal tooth development 
SubClassOf: MP:0002100 ## abnormal tooth morphology
```

#### CQ: morphology of developmental structure classifies under development ####

See above - this is somewhat inverted from the previous example:

```
Class: MP:0005650 ## abnormal limb bud morphology
SubClassOf: MP:0006279 ## abnormal limb development
```



See also: https://github.com/obophenotype/uberon/issues/346

### Traits ###

For our purposes here a trait is a phenotype without a value specified. Various modeling choices can be justified.

#### CQ: Traits connect to phenotypes ####

Example:
```
Class: 'small hippocampus'
SubClassOf: 'hippocampus size'
```

Note 1: here 'hippocampus size' refers to the size of the hippocampus in general. There is no assumption of abnormality

Note 2: other relationships beyond SubClassOf can be justified. However, there should be **some** connection between trait and phenotype ontologies (e.g. VT and MP) that can be inferred.

#### CQ: Traits are distinguishable ####

Example:
```
Class: 'eye color'
DisjointWith: 'eye shape'
```

Notes on E-centric approach:

If we were to model traits as:
```
Class: 'eye color' EquivalentTo: eye and has_quality some color
Class: 'eye shape' EquivalentTo: eye and has_quality some shape
```

Then given the background axioms:

```
Class: eye SubClassOf 'macroscopic material entity'
Class: 'macroscopic material entity'
  SubClassOf: has_quality some color, has_quality some shape
```

Then our two traits would collapse into (be entailed to be equivalent to) 'eye' leading to unsatisfiability of eye when the DisjointWith is added.

### Concentrations and populations ###

#### CQ: iron levels in spleen ####

Background: [Loebe et al](http://www.jbiomedsem.com/content/3/S2/S5) JBMS

Both

```
     is_a MP:0001770 ! abnormal iron level
       is_a MP:0008739 ! abnormal spleen iron level
        is_a MP:0008809 ! increased spleen iron level *** 
```

And

```
   is_a MP:0000689 ! abnormal spleen morphology
     is_a MP:0008739 ! abnormal spleen iron level
      is_a MP:0008809 ! increased spleen iron level ***
```

# See Also #

  * [properties of phenotype ontology modeling schemas](https://docs.google.com/spreadsheet/ccc?key=0AskRPmmvPJU3dC1pcjE5X1Y0UFJwVG1QRWhYYjdSRFE#gid=0)