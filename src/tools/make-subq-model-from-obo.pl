#!/usr/bin/perl

print "logical-definition-view-relation: has_part\n";
while (<>) {
    chomp;
    s/^(ontology:.*)/$1-subq/;
    print "$_\n";
}
