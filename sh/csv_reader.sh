#!/bin/bash

# Basic CSV file reading.

for line in `cat $1 | sed 's/ //g'`
do
  IFS=',' fields=($line)
  echo ${fields[0]}
done

