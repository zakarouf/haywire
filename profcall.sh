#!/bin/sh

make clean -j
make -j CXXFLAGS="-g -ggdb3 -flto -O3"
valgrind --tool=callgrind --callgrind-out-file=call.txt $*
cg_annotate --tree=both\
                   --inclusive=yes\
                   --auto=yes\
                   --show-percs=yes\
                   -I=src/ call.txt > cano.txt
less cano.txt
