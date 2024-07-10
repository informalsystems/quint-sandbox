#!/bin/bash

mkdir traces
quint run bank.qnt --mbt --n-traces=10000 --out-itf=traces/out.itf.json
cargo test
