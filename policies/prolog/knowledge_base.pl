:- module(knowledge_base, [knowledge_base_dict/1]).

:- use_module(['./policy.pl', './matrix.pl','./input.pl']).
modules([policy, policy_matrix, policy_input]).
knowledge_base_dict(Dict) :-
	findall((Name/Arity, P),
		(current_predicate(Name/Arity),
			functor(P, Name, Arity),
			(predicate_property(P,
					imported_from(Module)),
					modules(Modules),
				member(Module, Modules))%
				%  \+ predicate_property(P, built_in),
				%  \+ predicate_property(P, opaque),
				%  \+ predicate_property(P, dynamic),
				%  \+ predicate_property(P, discontiguous),
				%  \+ predicate_property(P, multifile),
				%  \+ predicate_property(P, tabled),
				%  \+ predicate_property(P, foreign),
				%  \+ predicate_property(P, cyclic_term)),
				),
		Predicates),
	maplist(predicate_props, Predicates, DictList),
	Dict = DictList.
    
    
predicate_props((Name/Arity, P), pred_dict{name:Name, arity:Arity, properties:Props, clauses:Clauses}) :-
	findall(Prop,
		predicate_property(P, Prop),
		Props),
	findall(ClauseDict,
		(nth_clause(P, _, Clause),
			clause_props(Clause, ClauseDict)),
		Clauses).

clause_props(Clause, clause_dict{head:HeadDict, body:BodyDict, properties:Props}) :-
	findall(Prop,
		clause_property(Clause, Prop),
		Props),
	clause(Head, Body, Clause),
	term_props(Head, HeadDict),
	term_props(Body, BodyDict).

% term_props(Term, term_dict{term_name:Name, arguments:Arguments, variables:Variables}) :-	
% (compound(Term),
% 	compound_name_arguments(Term, Name, Arguments),
% 		term_variables(Term, Variables));
% 		(
% 			Arguments = [],
% 			Variables = [],
% 			Name = Term).