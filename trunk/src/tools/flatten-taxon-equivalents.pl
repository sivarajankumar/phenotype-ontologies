#!/usr/bin/perl

while (<>) {

  #intersection_of: UBERON:0001707 ! nasal cavity
  #intersection_of: part_of NCBITaxon:10088

    if (/intersection_of: (UBERON:\d+)/) {
        print "equivalent_to: $1\n";
    }
    elsif (/intersection_of:/) {
    }
    else {
        print;
    }
    
}
