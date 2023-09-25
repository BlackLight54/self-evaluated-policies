#!/bin/bash

circom ancestor.circom --sym --wasm --r1cs
node ancestor_js/generate_witness.js ancestor_js/ancestor.wasm input_tree.json witness.wtns && echo "Witness generated"