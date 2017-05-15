#/bin/bash

./effstat.native -v > stat_raw.txt
sed -e '1,2d;$d' stat_raw.txt > stat_classifier.txt
cat stat_classifier.txt | tr -d : | while read line; do A=( $line ); for ((i=1;i<=${A[2]};i++)) do echo ${A[1]}; done; done > stat_size.txt
ministat stat_size.txt
