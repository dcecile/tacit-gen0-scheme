#!/usr/bin/env perl
use strict;
use warnings;

# Instructions: http://book.realworldhaskell.org/read/profiling-and-optimization.html
# Options: http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/prof-heap.html

system ("ghc --make -O2 SynopsJsonTest.hs -prof -auto-all -caf-all -fforce-recomp") == 0
  or die;
print "Starting\n";
system ("/usr/bin/time -p ./SynopsJsonTest db1000.xml.jsn +RTS -s -p -hc -i0.001");
system "hp2ps -c SynopsJsonTest.hp; ok SynopsJsonTest.ps";
