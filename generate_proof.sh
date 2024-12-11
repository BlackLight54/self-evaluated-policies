#!/usr/bin/env zsh
if ! [ -x "$(command -v circom)" ]; then
    echo -n "circom could not be found, installing"
    if ! [ -x "$(command -v cargo)" ]; then
      echo "cargo not found, installing"
      curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh
    fi
    git clone https://github.com/iden3/circom.git
    pushd circom || exit
      cargo build --release
      cargo install --path circom
    popd || exit
    rm -rf circom
fi
# check if packacge-lock.json or yarn.lock exists. If not, run npm install. Do this in a single if statement

if ! [ -f "package-lock.json" ] && ! [ -f "yarn.lock" ]; then
  echo "Lock file not found, running npm install"
  npm install
fi


echo -n "Generating proof tree..." \
&& ./policies/prolog/main.pl > tree.json \
&& echo "Ok" \
\
&& echo -n "Generating input tree and circuit..." \
&& pushd code-generator \
&& ./gradlew app:run  > /dev/null \
&& popd \
&& echo "Ok" \
&& echo -n "Generating witness for circuit ..." \
&& export  NODE_OPTIONS=--max_old_space_size=8192 \
&& circom --r1cs --wasm --c --sym --inspect generated.circom \
&& echo "Ok" \
&& echo -n "Generating proof..." \
&& snarkjs wtns calculate generated_js/generated.wasm input_tree.json witness.wtns \
&& echo "Ok"