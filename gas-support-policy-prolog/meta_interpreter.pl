:- include("./policy.pl").

% Proof tree
:- op(750, xfy, :>).

% end(leaf) of the proof tree
mi_proof_tree(true, [true]).
% conjunction
mi_proof_tree((A, B), Tree) :-
    !,
    mi_proof_tree(A, TreeA),
    mi_proof_tree(B, TreeB),
    append([TreeA, TreeB], Tree).
% disjunciton
mi_proof_tree((A; _), Tree) :-
    mi_proof_tree(A, Tree).
mi_proof_tree((_; B), Tree) :-
    mi_proof_tree(B, Tree).
% handle built-in predicates
mi_proof_tree(A, Tree) :-
    predicate_property(A, built_in),
    Tree = [[true] :> A],
    !,
    call(A).
% general case
mi_proof_tree(G, [Tree :> G]) :-
    G \= true,
    G \= (_,_),
    G \= (_\=_),
    clause(G, B),
    mi_proof_tree(B, Tree).

% simply print the proof tree to the terminal
print_proof_tree(A):- 
    mi_proof_tree(A, Tree),
    write("Proof tree: "), write(Tree), nl.


prove(A):-
    mi_proof_tree(A,Tree),
    print_tree(Tree).


% print a tree representation of the proof tree to the terminal
print_tree(Tree):-
    print_tree(Tree, 0).
print_tree([true], Indent):-
    tab(Indent), 
    write(true), nl.
print_tree([Items:>Goal], Indent):- 
    tab(Indent), 
    write(Goal), nl,
    NewIndent is Indent + 4,
    print_children(Items, NewIndent).
    % print_tree(NextItem, NewIndent).

print_children([], _).
print_children([Item|Items], Indent):- 
    print_tree([Item], Indent),
    print_children(Items, Indent).


