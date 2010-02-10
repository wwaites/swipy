:- module(skolemize,[skolemize/3]).

:- use_module(library('semweb/rdf_db'),[rdf_bnode/1]).

/**
 * Some unit tests
 */
:- begin_tests(skolemize).
test(skolemize) :-
	skolemize(
	[rdf_e(A,has,bnode(Nose)),rdf_e(bnode(Nose),colour,bnode(Colour)),rdf_e(bnode(Colour),label,pink)],
	[rdf_e(A,isa,man)],
	Skolem),!,
	Skolem==[rdf_e(A, has, individual(Nose, [A])), rdf_e(individual(Nose, [A]), colour, individual(Colour, [A])), rdf_e(individual(Colour, [A]), label, pink)].
:- end_tests(skolemize).
/**
 * End of unit tests
 */

skolemize(Head,Body,Skolem) :- 
	free_variables(Body,Vars),
	skolemize2(Vars,Head,Skolem),!.

skolemize2(_,[],[]).
skolemize2(Vars,[rdf_e(S,P,O)|T1],[rdf_e(individual(B,Vars),P,individual(C,Vars))|T2]) :-
	compound(S),S=bnode(B),((var(B),rdf_bnode(B));true),
	compound(O),O=bnode(C),((var(C),rdf_bnode(C));true),
	!,
	skolemize2(Vars,T1,T2).
skolemize2(Vars,[rdf_e(S,P,O)|T1],[rdf_e(individual(B,Vars),P,O)|T2]) :-
	compound(S),S=bnode(B),((var(B),rdf_bnode(B));true),
	!,
	skolemize2(Vars,T1,T2).
skolemize2(Vars,[rdf_e(S,P,O)|T1],[rdf_e(S,P,individual(B,Vars))|T2]) :-
        compound(O),O=bnode(B),((var(B),rdf_bnode(B));true),
	!,
        skolemize2(Vars,T1,T2).
skolemize2(Vars,[H|T1],[H|T2]) :-
	skolemize2(Vars,T1,T2).




