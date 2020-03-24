#! /bin/bash

cat userid-trackid.tsv | awk -F'\t' '{printf("%s\n",$1)}'| sort | uniq > users.txt

cat userid-trackid.tsv | awk -F'\t' '{printf("%s\n",$2)}'| sort | uniq > tracks.txt

awk '{printf("%1d\t%s\n", NR,$0)}' users.txt > userMap.tsv

awk '{printf("%1d\t%s\n", NR,$0)}' tracks.txt > trackMap.tsv

# note: manually moved the trackMap and userMap files to the correct folders