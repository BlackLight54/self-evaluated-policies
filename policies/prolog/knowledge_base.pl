:- module(knowledge_base, [knowledge_base_dict/1]).

:- use_module(library(http/json)).   % If you want JSON output in SWI-Prolog
:- use_module(['./policy.pl', 
               './matrix.pl',
               './input.pl'],[]).

%%----------------------------------------------------------------------------
%% 1) Top-level: produce a full knowledge base dictionary
%%----------------------------------------------------------------------------

modules([policy, policy_matrix, policy_input]).

knowledge_base_dict(KBDict) :-
    findall(ModuleDict,
		(
			% 1. Gather all relevant predicates from our module
			modules(Modules),
			member(Module, Modules),
			findall((Name/Arity, P),
        		( 
					current_predicate(Name/Arity),
          			functor(P, Name, Arity),
          			predicate_property(P, imported_from(Module))
        		),				
        		Predicates),
			% 2. Convert each predicate to a dictionary
    		maplist(predicate_props, Predicates, PredicateDicts),
    		% 3. Build a dictionary for the module
			ModuleDict = _{
				module_name : Module,
				predicates  : PredicateDicts
				}),
		ModuleDicts),

    KBDict = _{ modules: ModuleDicts }.

%%----------------------------------------------------------------------------
%% 2) Per-predicate: gather properties & clauses
%%----------------------------------------------------------------------------

predicate_props((Name/Arity, P), PredDict) :-
    % gather built-in properties if you like
    findall(Prop, predicate_property(P, Prop), Props),
    findall(ClauseDict,
        ( nth_clause(P, Index, ClauseRef),
          clause_props(ClauseRef, Index, P, ClauseDict)
        ),
        ClauseDicts),
    PredDict = _{
        predicate_name : Name,
        arity          : Arity,
        clauses        : ClauseDicts
        /* properties     : Props, */
    }.

%%----------------------------------------------------------------------------
%% 3) Per-clause: retrieve head and body structure
%%----------------------------------------------------------------------------

clause_props(ClauseRef, Index, _Pred, ClauseDict) :-
    % gather any additional clause properties
    findall(Prop, clause_property(ClauseRef, Prop), ClauseProps),
    % retrieve the actual Head/Body from the clause
    clause(Head, Body, ClauseRef),
    % numbervars so we can see variable identity (via $VAR(N))
    % This modifies Head/Body in place:
    numbervars((Head,Body), 0, _MaxVar, [attvar(bind)]),

    % convert body to a top-level list of goals
    body_to_list(Body, BodyGoals),

    % get structured info for each
    term_props(Head, HeadDict),
    maplist(term_props, BodyGoals, BodyDicts),

    ClauseDict = _{
       index     : Index,
       head      : HeadDict,
       body      : BodyDicts
       /* properties: ClauseProps, */
    }.

%%----------------------------------------------------------------------------
%% 4) Decompose a term into a nested dictionary
%%----------------------------------------------------------------------------

%% If it’s a module:goal, store module name separately.
%% If it’s a variable in numbervar form, store it as such.
%% If it’s atomic, just store it plainly.
%% If it’s compound, store the functor and a list of its arguments.
%% TODO: Handle lists: '[|]'(a,'[|]'(b,'[|]'(c,[])))
%% TODO: Handle empty lists : []

term_props(Term, Dict) :-
    ( Term = Module:Goal -> 
        term_props(Goal, GoalDict),
		Dict = GoalDict.put(_{module:Module})
	; Term = '$VAR'(N)   -> 
        Dict = _{ type: variable, number_in_clause: N }
	; var(Term)          -> 
        % In principle, if we didn't numbervars, we'd do something else here
        Dict = _{ type: var, value: "un-numbered-var" }
    ; atomic(Term)       -> 
        Dict = _{ type: atomic, value: Term }
    ; compound(Term)     -> 
        compound_name_arguments(Term, Functor, Args),
        maplist(term_props, Args, ArgDicts),
        Dict = _{
           type: compound,
           functor: Functor,
           arguments: ArgDicts
		   /* raw_term: Term */
        }
    ).

%%----------------------------------------------------------------------------
%% 5) Turn a clause body into a list of top-level goals
%%----------------------------------------------------------------------------

body_to_list((A, B), [A | Rest]) :-
    !,
    body_to_list(B, Rest).
body_to_list(true, []) :-
    !.
body_to_list(Goal, [Goal]).
