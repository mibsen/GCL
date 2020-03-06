#!/bin/sh

(cd .. && ./build.sh)

FILES=./cases/*
for f in $FILES
do
  echo "Processing $f file..."

  cat $f | tr -d "\n" | fsi.exe ../GCL.fsx      


done

