**THIS REPOSITORY HAS BEEN SPLIT INTO MULTIPLE GITHUB REPOS**

For details see:
https://github.com/obophenotype/upheno



The **phenotype-ontologies** repository aggregates OWL ontologies and associated inter-ontology relationships, including [LogicalDefinitions](LogicalDefinitions.md) for [human](HP.md), [mouse](MP.md), [zebrafish](ZP.md), [fly](DPO.md) and [worm](WBPhenotype.md), later to include [fission yeast](FYPO.md) and others.

For background, please see:

  * Sebastian Köhler, Sandra C Doelken, Barbara J Ruef, Sebastian Bauer, Nicole Washington, Monte Westerfield, George Gkoutos, Paul Schofield, Damian Smedley, Suzanna E Lewis, Peter N Robinson, Christopher J Mungall (2013) [Construction and accessibility of a cross-species phenotype ontology along with gene annotations for biomedical research](http://f1000research.com/articles/2-30/v1) F1000Research
  * C J Mungall, Georgios Gkoutos, Cynthia Smith, Melissa Haendel, Suzanna Lewis, Michael Ashburner (2010) [Integrating phenotype ontologies across multiple species](http://genomebiology.com/2010/11/1/R2) Genome Biology 11 (1).

# Getting Started #

Please see [GettingStarted](GettingStarted.md) for details on how to work with the OWL files

# OWL axiomatization #

The [OWLAxiomatization](OWLAxiomatization.md) page describes how we represent phenotypes in OWL, and how we reason with the resulting structures

# Applications #

The [Applications](Applications.md) page describes how the OWL axiomatization is used in applications such as [OWLSim](http://owlsim.org). This also describes the [CrossSpeciesPhenotypeOntology](CrossSpeciesPhenotypeOntology.md).

# Diseases #

The primary focus of this repository is on phenotypes, which we consider distinct from diseases. However the [DiseaseIntegration](DiseaseIntegration.md) page contains preliminary information on integrating and axiomatization of disease ontologies.

# Traits #

See [TraitsAndPhenotypes](TraitsAndPhenotypes.md).

# Automated builds and QC #

See the [build-pheno-ontologies](http://build.berkeleybop.org/job/build-pheno-ontologies) job on the Berkeley jenkins server

# Getting phenotype data #

Note that this repository is **not** intended to aggregate data - just the ontologies themselves. For the data, please see the respected sources, including:

  * http://human-phenotype-ontology.org/ (human)
  * http://informatics.jax.org/ (mouse)
  * http://wormbase.org (worm)
  * http://monarchinitiative.org (aggregated)