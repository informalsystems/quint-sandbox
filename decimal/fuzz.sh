#!/usr/bin/env bash
#
# A simple script for generating plenty of randomized tests with the Quint simulator.

# fail asap
set -e

for i in `seq 1 1000`; do
    echo "[$i] generating a long test..."
    quint run --max-samples=100 --max-steps=10000 --out-itf=t.itf.json decimalTest.qnt
    cp t.itf.json test-inputs-v0.46.4/oneRandom.itf.json
    echo "[$i] replaying the test..."
    cd go
    go test -v -run TestOneRun
    cd ..
done
