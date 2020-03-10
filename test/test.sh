#!/bin/sh

(cd .. && ./build.sh)

FILES=./cases/*
for f in $FILES
do
  echo "Processing $f file..."

  fsi.exe ../GCL.fsx $f     


done

