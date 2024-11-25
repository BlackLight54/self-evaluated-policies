# Self-Evaluating-Policies-w-ZKPs

## How to use

### Meta-interpreter
Open the relevant source files in SWI-Prolog, and run with the desired query.
```bash
swipl -s ./policies/prolog/meta_interpreter.pl -s ./policies/prolog/policy.pl -s ./policies/prolog/input.pl -s ./policies/prolog/matrix.pl -t "prove(inputPriceOk)."
```

or just run the `main.pl` script
```bash
./policies/prolog/main.pl
```

### Circom generator
0. Install the dependencies
    ```bash
    npm install
    ```

1. Put the proof tree in the correct spot
    ```bash
    ./policies/prolog/main.pl > tree.json
    ```

2. Run the java program to generate the circom code and the input file
    ```bash
    ./code-generator/gradlew app:run
    ```

3. Compile the R1CS circuit, with witness generator 
    ```bash
    circom --r1cs --wasm --c --sym --inspect code-generated.circom
    ```

4. View information about the R1CS circuit
    ```bash
    NODE_OPTIONS=--max_old_space_size=8192;snarkjs r1cs info generated.r1cs
    ```
    From this you can see the number of constrains and variables, which is useful for the next step. For the example policy, we had  1.327.402 constraints.
    
5. download the appropriate [powers of tau ceremony file](https://github.com/iden3/snarkjs?tab=readme-ov-file#7-prepare-phase-2)
    ```bash
    wget https://hermez.s3-eu-west-1.amazonaws.com/powersOfTau28_hez_final_21.ptau
    ```
6. (OPTIONAL) Calculate the witness    
    ```bash
    snarkjs wtns calculate generated_js/generated.wasm input_tree.json witness.wtns
    ```
   Optionally, you can check if it complies with the R1CS constraints
    ```bash
    snarkjs wtns check generated.r1cs witness.wtns    
    ```
7. Perform the trusted setup
    ```bash
   snarkjs plonk setup generated.r1cs powersOfTau28_hez_final_21.ptau circuit_final.zkey
    ```
   You can verify the zkey file with:
    ```bash
    snarkjs zkey verify generated.r1cs powersOfTau28_hez_final_21.ptau circuit_final.zkey
    ```
8. Export the verification key
    ```bash
    snarkjs zkey export verificationkey circuit_final.zkey verification_key.json
    ```
9. Generate the proof
    ```bash
    snarkjs plonk fullprove input_tree.json generated_js/generated.wasm circuit_final.zkey proof.json public.json
    ```
10. Verify the proof
    ```bash
    snarkjs plonk verify verification_key.json public.json proof.json
    ```

## Meta-interpreter
Sources: 
1. [The Power Of Prolog: A Couple of Meta-interpreters in Prolog](https://www.metalevel.at/acomip/)

### Goal 
The goal is to prove the evaluation of a Prolog program. This can be done using a proof tree.\[?\]
To do this, we must interpret the Prolog program that is being queried, and extract the states that are the nodes of the proof tree.

### Interpreters
"Informally, an interpreter is a program that evaluates programs. Interpretation is pervasive in computer science both from a theoretical and practical perspective, and many programs are interpreters for domain-specific languages. For example, a program reading settings from a configuration file and adjusting itself accordingly is interpreting this "configuration language". "\[1\]

### Meta-interpreters; Meta-circular interpreters
"An interpreter for a language similar or identical to its own implementation language is called meta-interpreter (MI). An interpreter that can interpret itself is called meta-circular."

### Why is Prolog especially well suited for meta-interpretation
 - "First and most importantly, Prolog programs can be naturally represented as Prolog terms and are easily inspected and manipulated using built-in mechanisms"; "Prolog is a *homoiconic* language. Other homoiconic languages: Lisp, machine language, etc."

 - "Second, Prolog's implicit computation strategy and all-solutions predicates can be used in interpreters, allowing for concise specifications."; "Other languages with this property: Tcl, PostScript "

 - "Third, variables from the object-level (the program to be interpreted) can be treated as variables on the meta-level (the interpreter); therefore, an interpreter can delegate handling of the interpreted program's binding environment to the underlying Prolog engine."; i.e."Prolog is a very simple language; only construct: Head <- Body 

 "The alignment of these specific features is unique to Prolog." \[1\]

 ### Definitions 
 - object-level: the level of the program being interpreted
 - meta-level: the level of the meta-interpreter
 - meta-circular: a meta-interpreter that can interpret its own source code
 - absorption: a meta-interpreter uses an implicit language feature
 - reification: a meta-interpreter makes a language feature explicit, observable
 - meta-call: when a goal is dynamically invoked: `Goal. call(Goal).`. This is available in Prolog out of the box.
 - reflection and introspection: a program's ability to examine itself, available in many other languages, e.g. Java. In Prolog, the main tool for introspection among others, which is also a meta-call: `clause(Goal, Body)`

### clause/2
True if Head can be unified with a clause head and Body with the corresponding clause body. \[[SWIPL docs](https://www.swi-prolog.org/pldoc/doc_for?object=clause/2)\]
```prolog
h(X,Y):- f(X) f(Y).
h(u,v).

?-  clause(h(A,B), Body).
    Body = (f(A),g(B))          % unification is implicit; is absorbed
;   Body = true, A = u, B = v.
```

### Vanilla meta-interpreter
"vanilla": ordinary, standard
```prolog
mi(true).                               % because mi(G) :- G = true. always holds, this is the base case
mi((A,B)) :- mi(A),mi(B).               % conjunction case (`a,b.`)
mi(G,Body) :-                           % general case 
    clause(G,Body),
    mi(Body).  
```
This is the most basic meta-interpreter in Prolog, it reifies conjunction, but it is sub-optimal because it still absorbs most language functions, leads to false choice points (because `mi(true).` unifies with both the first and the third clause ), and cannot handle built-in predicates (because they are private, often precompiled in the specific Prolog implementations). 

Still, it shows the basic structure of Prolog meta-interpreters. We will expand on this framework of meta-interpretation to cosntruct our proof tree, specifically will reify disjunction unification, substitution, built-in predicates, and organize these observations into a tree data structure. We may also reify backtracking, but it isn't necessary, as our use-case calculates a single value. 

Prolog meta-interpreters can be greatly improved in both performance and complexity (even as short as 2 clauses) by modifying the representation of the object-level program, as shown by \[1\].

### Meta-interpreter constructing a proof-tree

Simply store that a given body implies a given goal
```prolog 
% here: ?- mi(Goal,ProofTree). 
:- op(500,xfy, =>).

mi(true,true).
mi((A,B),(PA,PB)) :- mi(A,PA), mi(B,PB).
mi(G, P => G) :- clause(G, Body), mi(Body,P).
```

### Our Structured meta-interpreter
- Reifies: conjunction, disjunction, built-ins, unification, substitution
- Stores the proof tree in a nested dictionary
```json 
{
    
    "goal":"originalGoal(X)",
    "substitution": [],
    "unification": {
        "body": [
            "unifiedPred1(_7978)",
            "unifiedPred2(_7968,_7978)",
        ],
        "goal":"originalGoal(_7968)"
    },
    "ztree": [
        {        
            "goal":"unifiedPred1(X)",
            "substitution": [...],
            "unification": {...},
            "ztree": [
                ...
                ...
                ...
            ]
        },
        ...
        ...
        ...
    ]
}
```

Here, the evaluation of the Prolog program can be traced, because the exact unfications and substituions are reified and recorded, and can be replayed.

### Related 

- Interpretation is pervasive
- completeness results (NP-completeness, Turing-completeness)
- incompleteness results (Gödel's first incompleteness thorem: "compilign" a proof system for natural numbers)
- most programs are interpreters for specific languages
- **CLPZ for scryer prolog: dedicated interpreted language for formulating CLP Integer problems** 
- Code is data; most programs interpret code in a different language e.g. browser interprets HTML,JS,CSS