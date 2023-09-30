# Self-evaluating policies using zero knowledge proofs

## Abstract

- Tételmodat: kontextusteremtés

- absztrakt problémafelvetés

- Megoldás

- Milyen megoldást javaslunk

- Ez miért jó? Generikus. Miért prolog? Kifejező ereje nagy. Miért ZKP? 

- Potenciálisan CLP(Korlátkielégítés logikai programozás)

- konkrét use-case-en prototípuson keresztül bemutatjuk a megoldásunkat

- Már létező de kezdeti megközelítésekkel szemben

## Introduction

## Background

### Prolog

what is prolog? what are declarative languages? How is a prolog query evaluated? How can we prove the evaluation of a prolog query?

### Meta-interpretation

What is interpretation? What is meta-interpretation? How is it related to prolog, why is it a good candidate? How can we construct proof of evaluation using a meta-interpreter?

### Zero knowledge proofs, how do they typically work?

### Zero knowledge frameworks for proving program exectuion, using R1CS

Circuitree -> Bulletproofs -> R1CS -> Circom

### Existing use-cases for ZKPs in SSI and DLT ecosystems

#### Brief introductrion to SSI

#### Brief introduction to DLTs

### Related works

- "Bár léteznek datalog alapú rendszerek, ezeak azonban korlátozottal a következő módon, ezzel szeben a mi munkánk...

## Motivation

### Policies over SSI credentials using declarative languages

- exsisting: walt.id and OPA

### Self-evaluating policies as security and privacy guarantees

### Privacy preserving polcies using ZKPs

## Approach

### Proof of prolog evaluation with public program and private input

- Design of the Meta-Interpreter

	- limitations, sub language defintion: no cut, no negation

- Implementation

### Efficient representation of prolog proof-trees

### Algorithm for verifying prolog proof trees

### ZKP framework for proving a prolog evaluation with public program and private input 

- intro

	- Future work: recursive proofs

	- IDEA: construct a circuit that is able to prove inputs of a specific prolog program, and then use it to prove the correctness of the proof tree. This circuit is designed in suh a way that it can be generated from the prolog program itself.

- Architecture

- Designing circuits 

	- Alice ancestor example

- Generating circuits for a specific prolog program

	- Design of the generator

	- Implementation

## Results

### Konrét use-case-t teszteljük: Alice ancestor example

### Validtation and analisys 

### Performance evaluation

## Discussion

## Conclusion

## Future work

