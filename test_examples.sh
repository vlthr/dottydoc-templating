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

# commasep=$(IFS=, ; echo "${astfiles[*]}")
# scalafmt -i --files $commasep
if [[ $REGENERATE ]]; then
    echo "Regenerating expected outputs..."
    for f in ${liquidfiles[@]}; do
        base="${f%.*}"
        ast="$base.ast"
        parseTree="$base.parseTree"
        echo $base ...
        cp $ast $ast-expected
        cp $parseTree $parseTree-expected
    done
else
    echo "Comparing generated files with expected outputs..."
    for f in ${liquidfiles[@]}; do
        base="${f%.*}"
        ast="$base.ast"
        parseTree="$base.parseTree"
        echo -n $base ...
        parseTree_diff=$(diff $parseTree $parseTree-expected)
        if [ $? -eq 0 ]; then
            echo "parse tree correct!"
        else
            echo "parse tree mismatch!"
            echo "${parseTree_diff}"
        fi
        ast_diff=$(diff $ast $ast-expected)
        if [ $? -eq 0 ]; then
            echo "AST correct!"
        else
            echo "AST mismatch!"
            echo "${ast_diff}"
        fi
    done
fi
