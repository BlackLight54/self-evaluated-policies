:- include("./policy.pl").

% operator for proof tree nodes,
% means that the left side is a list of subgoals and the right side is the goal,
% i.e. [subgoal1, subgoal2, ...] :> goal <> unification >< substitution <> unification
:- op(750, xfy, :>). 
:- op(751, xfy, <>).
:- op(752, xfy, ><).

% Meta-interpreter
%
% base case, end(leaf) of the proof tree
mi_proof_tree(true, [true]).
% conjunction
mi_proof_tree((A, B), Tree) :-
    !,
    mi_proof_tree(A, TreeA),
    mi_proof_tree(B, TreeB),
    append([TreeA, TreeB], Tree). % is append part of ISO prolog? If not, its source is just a few clauses
% disjunciton
mi_proof_tree((A; _), Tree) :-
    mi_proof_tree(A, Tree).
mi_proof_tree((_; B), Tree) :-
    mi_proof_tree(B, Tree).
% handle built-in predicates
mi_proof_tree(A, Tree) :-
    predicate_property(A, built_in),
    U = uni,
    S = sub,
    Tree = [[true] :> A <> U >< S],
    !,
    call(A).
% general case
mi_proof_tree(G, [Tree :> G <> U >< S]) :-
    % G \= true, %  predicate_property(A, built_in) already filters these, but i don't know if its part of ISO prolog
    % G \= (_,_),
    % G \= (_\=_), 
    clause(G, B),
    S = sub,
    U = uni,
    mi_proof_tree(B, Tree).

% simply print the proof tree to the terminal
print_proof_tree(A):- 
    mi_proof_tree(A, Tree),
    write("Proof tree: "), write(Tree), nl.

prove(A):-
    mi_proof_tree(A,Tree),
    print_tree_pretty(Tree).

% write a JSON representation of the proof tree to the terminal
print_tree(Tree):-
    print_tree(Tree, 0).
print_tree([true], Indent):-
    tab(Indent),write("{"),nl,
    tab(Indent),tab(2),write("\"goal\":true"), nl,
    tab(Indent),write("}").
print_tree([ Children :> Goal <> U >< S], Indent):- 
    tab(Indent),write("{"),nl,
    tab(Indent),tab(2),write("\"goal\": \""),write(Goal),write("\","),nl,
    tab(Indent),tab(2),write("\"unification\": \""),write(U),write("\","),nl,
    tab(Indent),tab(2),write("\"substitution\": \""),write(S),write("\","),nl,
    tab(Indent),tab(2),write("\"children\": ["), nl,
    NewIndent is Indent + 4,
    print_children(Children, NewIndent),nl,
    tab(Indent),tab(2),write("]"), nl,
    tab(Indent),write("}").

print_children([], _).
print_children([Child],Indent):-
    print_tree([Child], Indent).
print_children([Child|OtherChildren], Indent):- 
    nl,
    print_tree([Child], Indent),
    write(","),
    print_children(OtherChildren, Indent).

% print a tree representation of the proof tree to the terminal
print_tree_pretty(Tree):-
    print_tree_pretty(Tree, 0).
print_tree_pretty([true], Indent):-
    tab(Indent),
    write("< ") 
    write(true), nl.
print_tree_pretty([ Children :> Goal <> U >< S], Indent):- 
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



