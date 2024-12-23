#!/bin/sh
make debug && make && lldb ./bin/hw a b abc && make clean
