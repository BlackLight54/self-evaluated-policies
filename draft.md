# Self-evaluating policies using zero knowledge proofs
## Abstract
Tételmodat: kontextusteremtés
absztrakt Problémafelvetés
Megoldás
Milyen megoldást javaslunk
Ez miért jó? Generikus. Miért prolog?Miért nem datalog? Miért ZKP? Miért R1CS és Circom?
"Bár léteznek datalog alapú rendszerek, ezeak azonban korlátozottal a következő módon, ezzel szeben a mi munkánk...
konkrét use-case-en prototípuson keresztül bemutatjuk a megoldásunkat
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

## Motivation

### Policies over SSI credentials using declarative languages

exisiting: walt.id and OPA

### Self-evaluating policies as security and privacy guarantees

### Privacy preserving policies using ZKPs

## Constructing a proof tree of a prolog evaluation with a public program and private input

### Desiogn of the Meta-interpreter

limitations, sub language defintion: no cut, no negation

### Implementaion



## Efficient representation of prolog proof trees

### Theoretical idea

### Implementation

## Algorithm for verifying a prolog proof tree

## Zero-knowledge framework for proving the correctness of a prolog proof tree

Future work: recursive proofs

IDEA: construct a circuit that is able to prove inputs of a specific prolog program, and then use it to prove the correctness of the proof tree. This circuit is designed in suh a way that it can be generated from the prolog program itself.

### Architecture

### How can we design circuits that can prove the correctness of a prolog proof tree?

Alice ancestor example

### How can we generate such circuits from prolog programs?

#### Design of the generator program

## Results

Konrét use-case-t teszteljük: Alice ancestor example

### 

### Performance evaluation



## Discussion
