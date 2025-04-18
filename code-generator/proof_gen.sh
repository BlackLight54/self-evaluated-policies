#!/bin/bash

# This script generates the proof for the circuit in the file
if  ! test -f "powersOfTau28_hez_final_22.ptau"; then
    echo "Downloading powersOfTau28_hez_final_22.ptau"
    wget https://hermez.s3-eu-west-1.amazonaws.com/powersOfTau28_hez_final_22.ptau
fi
#./gradlew app:run
circom policy.circom --r1cs --wasm -l ../
startPlonkSetup=$(date +%s)
snarkjs wtns calculate policy_js/policy.wasm app/input_tree.json witness.wtns
echo "Plonk setup start time: $startPlonkSetup"
node --max-old-space-size=51200 /usr/local/bin/snarkjs plonk setup policy.r1cs powersOfTau28_hez_final_22.ptau circuit_final.zkey
endPlonkSetup=$(date +%s)
echo "Plonk setup time: $((endPlonkSetup-startPlonkSetup))s"

time snarkjs plonk prove circuit_final.zkey witness.wts proof.json public.json