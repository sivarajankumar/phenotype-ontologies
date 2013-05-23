#!/usr/bin/perl

my @hdr = ();
while ($ARGV[0] && $ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-h') {
        print usage();
    }
    elsif ($opt eq '-i' || '--ont') {
        @hdr = ("ontology: ".shift(@ARGV));
    }
    else {
        die "unknown option: $opt";
    }
}

print "$_\n" foreach @hdr;
print "\n";

while (my $f = shift @ARGV) {
    
    #intersection_of: UBERON:0001707 ! nasal cavity
    #intersection_of: part_of NCBITaxon:10088
    my $h = 1;

    open(F,$f) || die "no file $f";
    print STDERR "READING: $f\n";
    while(<F>) {

        if (/^\[/) {
            $h = 0;
        }
        next if $h;

        if (/intersection_of: (UBERON:\d+)/) {
            print "equivalent_to: $1\n";
        }
        elsif (/intersection_of: (CL:\d+)/) {
            print "equivalent_to: $1\n";
        }
        elsif (/intersection_of:/) {
        }
        else {
            print;
        }
    }
    close(F);
    
}
