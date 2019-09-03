#!/bin/bash
# Script to launch each KAT test for round5

echo "Testing all the KAT files"
files=$(ls ./src/test/scala)
pattern="^KATTest_"
for entry in $files;
do
  if [[ $entry =~ $pattern ]]
  then
    name=$(echo $entry | cut -f 1 -d '.')
    echo $name
    sbt "testOnly *$name"
  fi
done
echo "End of test"