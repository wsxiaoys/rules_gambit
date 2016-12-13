#!/bin/sh

for dir in $(ls tests); do
        bazel run tests/$dir/main
done
