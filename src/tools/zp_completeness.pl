#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use feature 'say';

my $outFile = "zp_report.txt";

open (ZPOWL,"<zp.owl") or die $!;
open (UPHENO,"<vertebrate-merged-simple.obo") or die $!;

open (OUT,">$outFile") or die $!;

my $zpowl = {};
my $upheno = {};
my $both = {};

while (<ZPOWL>) {
 	chomp;
	my $line = $_;
	
	if ($line =~/(ZP_\d+)/){
		$zpowl->{$1}=1;
		$both->{$1}=1;
	}
	
}
while (<UPHENO>) {
 	chomp;
	my $line = $_;
	
	if ($line =~/(ZP:\d+)/){
		my $zp = $1;
		$zp =~ s/:/_/;
		$upheno->{$zp}=1;
		$both->{$zp}=1;	
	}
	
}

for my $both ( keys %$both ) {
	unless ($zpowl->{$both}){
		print OUT "$both missed in zpowl / found in uberpheno\n";
	}

	unless ($upheno->{$both}){
		print OUT "$both missed in uberpheno / found in zpowl\n";
	}
}

close (ZPOWL); 
close (UPHENO); 
close (OUT);

