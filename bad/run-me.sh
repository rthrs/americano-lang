#!/bin/bash

if [ ! -f ../interpreter ]; then
    echo "Build project first!"
    exit 1
fi

for file in *.am
do
  echo "-- $file --------------------------------------------------------------" 
  ../interpreter $file
  echo -e ""
done
