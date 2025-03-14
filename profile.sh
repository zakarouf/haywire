#!/bin/sh

make clean -j
make -j CXXFLAGS="-g -ggdb3 -flto -O3"
valgrind --tool=callgrind --callgrind-out-file=call.txt $*
callgrind_annotate --tree=both --inclusive=yes --auto=yes --show-percs=yes call.txt > cano.txt
rm call.txt
less cano.txt
