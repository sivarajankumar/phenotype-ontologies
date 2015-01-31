# Automated translation/rewrites

## Installation

 * https://github.com/cmungall/owljs

## Running

Either run from parent Makefile:

    cd ..
    make mp/mp-edit-tr.owl

Or run individual translations:

    owljs-repl -l tr/hasPart.js mp/mp-edit.owl

## How it works

It makes use of the DLMatch library in owljs. Each script has function
calls to the replace function which performs a query for a matching
pattern, and then generates axioms using the results of the query.

## TODO

Integrate with
https://docs.google.com/document/d/1LOYQ6pOlvapzJhHbSA_Rb3aZYylXZ564B78jiysvB-U/edit#

