#!/bin/sh
make clean
make prof
valgrind --tool=cachegrind\
         --cachegrind-out-file=cg.txt\
                ./prf/hw -n -c main examples/hwasm/03_recursion.hws

cg_annotate cg.txt | less 

