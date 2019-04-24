#!/bin/bash
set -e

stack build --fast
for filename in hw3/test/task2/*.sh; do
    echo "$filename running..."
    stack exec -- mini-shell "$filename" $*
    printf "\n"
done
