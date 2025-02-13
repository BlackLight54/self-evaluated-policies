:- module(knowledge_base, [knowledge_base_dict/1]).

:- use_module('./policy.pl').
:- use_module('./matrix.pl').
:- use_module('./input.pl').

knowledge_base_dict(Dict) :-
	findall((Name/Arity, P),
		(current_predicate(Name/Arity),
			functor(P, Name, Arity),
			(predicate_property(P,
					imported_from(policy));
predicate_property(P,
					imported_from(policy_matrix));
predicate_property(P,
					imported_from(policy_input)))%  \+ predicate_property(P, built_in),
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
    
    
predicate_props((Name/Arity, P), Dict) :-
	findall(Prop,
		predicate_property(P, Prop),
		Props),
	findall(_{
			head : Head,
			body : Body,
			properties : ClauseProps
			},
		(nth_clause(P, _, Clause),
			findall(ClauseProp,
				clause_property(Clause, ClauseProp),
				ClauseProps),
			clause(Head, Body, Clause)),
		Clauses),
	Dict = _{
		name : Name,
		arity : Arity,
		properties : Props,
		clauses : Clauses
		}.

