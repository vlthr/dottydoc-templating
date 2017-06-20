#!/bin/bash
while [[ $# -ge 1 ]]
do
    key="$1"
    case $key in
        -r |--regenerate)
            REGENERATE="yes"
            shift
            ;;
        *)
            # unknown option
            ;;
    esac
    shift
done

shopt -s failglob
liquidfiles=(./app/src/test/resources/examples/*.liquid)
spacesep="${liquidfiles[@]}"
sbt "genTestExamples $spacesep"

astfiles=(./app/src/test/resources/examples/*.ast)
# commasep=$(IFS=, ; echo "${astfiles[*]}")
# scalafmt -i --files $commasep
if [[ $REGENERATE ]]; then
    echo "Regenerating expected outputs..."
    for f in ${astfiles[@]}; do
        echo $f ...
        cp $f $f-expected
    done
else
    echo "Comparing generated files with expected outputs..."
    for f in ${astfiles[@]}; do
        echo -n $f ...
        d=$(diff $f $f-expected)
        if [ $? -eq 0 ]; then
            echo "correct!"
        else
            echo "mismatch!"
            echo "${d}"
        fi
    done
fi
