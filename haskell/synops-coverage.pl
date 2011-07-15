#!/usr/bin/env perl
use strict;
use warnings;

system ("rm SynopsJsonTest.tix; ghc --make -fhpc SynopsJsonTest.hs -fforce-recomp") == 0
  or die;
print "Starting\n"; 
system ("/usr/bin/time -p ./SynopsJsonTest db100.xml.jsn");
system "hpc markup --fun-entry-count SynopsJsonTest";
