#!/bin/sh
make debug && make && lldb ./debug/hw a b abc --one-line r --one-line gui && make clean
