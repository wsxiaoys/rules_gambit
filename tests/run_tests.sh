#!/bin/sh

for dir in $(ls -d tests/*/); do
        bazel run $dir/main
done
