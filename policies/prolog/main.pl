#!/usr/bin/swipl -q

:- use_module(library(http/json)).
:- include('./policy.pl').
:- include('./matrix.pl').
:- include('./input.pl').
:- include('./meta_interpreter.pl').
:- initialization main.


main:-
    prove(inputPriceOk,[Tree])
    ,json_write_dict(current_output, Tree,[width(100),serialize_unknown(true)])
    ,halt
    .


% simply print the proof tree to the terminal
print_proof_tree(A):-
    write("Proof tree for: "), write(A), nl,
    prove(A, [Tree]),
    write("Proof tree: "), write(Tree), nl.

% print a tree representation of the proof tree to the terminal
print_tree_pretty(Tree):-
    print_tree_pretty(Tree, 0).
print_tree_pretty([true], Indent):-
    tab(Indent),
    write("< "),
    write(true), nl.
print_tree_pretty([{goal:Goal,goal_unification:U,substitution:S,subtree:Children}], Indent):-
    tab(Indent),
    write( ":> "),
    write(Goal),
    write(" <> "),
    write(U),
    write(" >< "),
    write(S),
    nl,
    NewIndent is Indent + 4,
    print_children_pretty(Children, NewIndent).

print_children_pretty([], _).
print_children_pretty([Child|OtherChildren], Indent):-
    print_tree_pretty([Child], Indent),
    print_children_pretty(OtherChildren, Indent).