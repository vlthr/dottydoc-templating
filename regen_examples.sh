#!/bin/bash
shopt -s failglob
liquidfiles=(./examples/*.liquid)

echo "Regenerating expected outputs..."
for f in ${liquidfiles[@]}; do
    base="${f%.*}"
    echo $base ...
    for output in "$base".{parseTree,render}; do
        cp $output $output-expected
    done
done
