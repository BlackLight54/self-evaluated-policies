#!/usr/bin/env bash
./policies/prolog/main.pl > tree.json \
&& pushd code-generator \
&& ./gradlew app:run \
&& popd \
&& export  NODE_OPTIONS=--max_old_space_size=8192 \
&& circom --r1cs --wasm --c --sym --inspect generated.circom \
&& snarkjs wtns calculate generated_js/generated.wasm input_tree.json witness.wtns