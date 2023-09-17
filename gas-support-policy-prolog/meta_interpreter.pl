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
mi_proof_tree(Goal, [[true] :> Goal <> [] >< Substitution]) :-
    predicate_property(Goal, built_in), % is this prperty part of ISO prolog?
    % U = [],
    % S = [],
    % Tree = [[true] :> A <> U >< S],
    !,
    call(Goal),
    copy_term(Goal, NewGoal),
    NewGoal = Goal,
    findall((Var, Value), (member(Var, Goal), Var = Value), Substitution).

% general case
mi_proof_tree(Goal, [Tree :> Goal <> Unification >< Substitution]) :-
    % Goal \= true, %  predicate_property(A, built_in) already filters these, but i don't know if its part of ISO prolog
    % Goal \= (_,_),
    % Goal \= (_\=_), 
    clause(Goal, Body),
    copy_term((Goal, Body), (NewGoal, NewBody)),
    NewGoal = Goal,
    unification_trace(Body, NewBody, Unification),
    mi_proof_tree(Body, Tree),
    findall((Var, Value), (member(Var, Goal), Var = Value), Substitution).

    unification_trace(A, B, []) :- var(A), var(B), !.
unification_trace((A, C), (B, D), [(A, B), (C, D)]) :- !.
unification_trace(A, B, [(A, B)]).






% simply print the proof tree to the terminal
print_proof_tree(A):- 
    mi_proof_tree(A, Tree),
    write("Proof tree: "), write(Tree), nl.

prove(A):-
    mi_proof_tree(A,Tree),
    print_tree_JSON(Tree).

% write a JSON representation of the proof tree to the terminal
print_tree_JSON(Tree):-
    print_tree_JSON(Tree, 0).
print_tree_JSON([true], Indent):-
    tab(Indent),write("{"),nl,
    tab(Indent),tab(2),write("\"goal\":true"), nl,
    tab(Indent),write("}").
print_tree_JSON([ Children :> Goal <> U >< S], Indent):- 
    tab(Indent),write("{"),nl,
    tab(Indent),tab(2),write("\"goal\": \""),write(Goal),write("\","),nl,
    tab(Indent),tab(2),write("\"unification\": \""),write(U),write("\","),nl,
    tab(Indent),tab(2),write("\"substitution\": \""),write(S),write("\","),nl,
    tab(Indent),tab(2),write("\"children\": ["), nl,
    NewIndent is Indent + 4,
    print_children_JSON(Children, NewIndent),nl,
    tab(Indent),tab(2),write("]"), nl,
    tab(Indent),write("}").

print_children_JSON([], _).
print_children_JSON([Child],Indent):-
    print_tree_JSON([Child], Indent).
print_children_JSON([Child|OtherChildren], Indent):- 
    nl,
    print_tree_JSON([Child], Indent),
    write(","),
    print_children_JSON(OtherChildren, Indent).

% print a tree representation of the proof tree to the terminal
print_tree_pretty(Tree):-
    print_tree_pretty(Tree, 0).
print_tree_pretty([true], Indent):-
    tab(Indent),
    write("< "),
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



