% mi(true,_, _).
% mi((A, B), N, Trace) :-
%     mi(A, N, TraceA),
%     length(TraceA, L),
%     M is N + L, 
%     mi(B, M, TraceB),
%     append(TraceA, TraceB, Trace).
% mi(Goal, N,[(N, Goal :- Body, Unification)|Trace]) :-
%     Goal \= true,
%     Goal \= (_,_),
%     clause(Goal, Body),
%     NewGoal = Goal,
%     copy_term((Goal, Body), (NewGoal, NewBody)),
%     unification_trace(Body, NewBody, Unification),
%     M is N + 1,
%     mi(Body, M, Trace).



% Proof tree
% ?- mi_proof_tree(ancestor(anne,carol),T). 
% T = ((true=>parent(anne, bob), (true=>parent(bob, carol))=>ancestor(bob, carol))=>ancestor(anne, carol)) 
:- op(750, xfy, =>).

mi_proof_tree(true, true).
mi_proof_tree((A, B), (TreeA, TreeB)) :-
    !,
    mi_proof_tree(A, TreeA),
    mi_proof_tree(B, TreeB).

% handle built-in predicates
mi_proof_tree(A, Trace) :-
    predicate_property(A, built_in),
    Trace = (true => A),
    !,
    call(A).
mi_proof_tree(G, Tree => G) :-
    G \= true,
    G \= (_,_),
    G \= (_\=_),
    clause(G, B),
    mi_proof_tree(B, Tree).


unification_trace(A, B, []) :- var(A), var(B), !.
unification_trace((A, C), (B, D), [(A, B), (C, D)]) :- !.
unification_trace(A, B, [(A, B)]).









prove(A) :-
    mi_proof_tree(A,Trace),
    print_trace(Trace).

% print_trace([]).
% print_trace([(N, A :- B, Substitution)|Trace]) :-
%     write("Step:"), write(N),nl,
%     write(A), write(" :- "), write(B), write(" "), write(Substitution), nl,
%     print_trace(Trace).

print_trace(Trace):- 
    write("Proof tree: "), write(Trace), nl.



:- include("./policy.pl").
:- include("./testprograms.pl").