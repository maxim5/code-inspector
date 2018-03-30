#!/usr/bin/env bash
wget http://www.cs.virginia.edu/stream/FTP/Code/stream.c
gcc -O2 -msse -msse2 -msse3 -o stream stream.c -static -fopenmp
export OMP_NUM_THREADS=32
./stream
export OMP_NUM_THREADS=8
export GOMP_CPU_AFFINITY="0 1 2 3 4 5 6 7"
./stream
