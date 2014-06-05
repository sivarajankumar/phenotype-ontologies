#!/bin/sh
ONT=../src/ontology
owltools --catalog-xml $ONT/catalog-v001.xml all.owl --use-fsim  --sim-load-lcs-cache owlsim.cache --sim-load-ic-cache ic-cache.owl --start-sim-server -p 9031
#####OWLTOOLS_MEMORY=12G owltools --catalog-xml $ONT/catalog-v001.xml all.owl --use-fsim --sim-load-ic-cache ic-cache.owl --sim-load-lcs-cache owlsim.cache  --start-sim-server -p 9031
####owltools --catalog-xml $ONT/catalog-v001.xml $ONT/mammal.owl --merge-imports-closure --load-instances $MM/Mm-gene-to-phenotype-BF.txt --load-labels $MM/Mm-gene-labels.txt  --fsim-compare-atts -p owlsim-cache.properties -o owlsim-cache.full --start-sim-server -p 9031
