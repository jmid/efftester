#/bin/bash

./effstat.native -v > stat_raw.txt
tail -r stat_raw.txt | tail +2 | tail -r | tail +3 > stat_classifier.txt
cat stat_classifier.txt | tr -d : | while read line; do A=( $line ); for ((i=1;i<=${A[2]};i++)) do echo ${A[1]}; done; done > stat_height.txt
ministat stat_height.txt
