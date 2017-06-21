#!/bin/bash
shopt -s failglob
liquidfiles=(./app/src/test/resources/examples/*.liquid)

echo "Regenerating expected outputs..."
for f in ${liquidfiles[@]}; do
    base="${f%.*}"
    echo $base ...
    for output in "$base".{ast,parseTree}; do
        cp $output $output-expected
    done
done
