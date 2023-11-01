#!/bin/bash

# This script generates the proof for the circuit in the file
if  ! test -f "powersOfTau28_hez_final_16.ptau"; then
    echo "Downloading powersOfTau28_hez_final_10.ptau"
    wget https://hermez.s3-eu-west-1.amazonaws.com/powersOfTau28_hez_final.ptau
fi

snarkjs plonk setup generated.r1cs powersOfTau28_hez_final_21.ptau circuit_final.zkey

time snarkjs plonk prove circuit_final.zkey witness.wts proof.json public.json