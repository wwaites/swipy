:- module(rdf_e,[rdf_e/3,cache/3]).

:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
                rdf_bnode/1,
                rdf_global_term/2,
		rdf_is_bnode/1,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module(library('semweb/rdfs'),
              [ rdfs_list_to_prolog_list/2 ]).
:- use_module(builtins).
:- use_module(utils,
              [ list_to_conj/2,
	        get_list/2,
	        pl_list_to_rdf_list/3
	      ]).

:- dynamic rdf_e/3.


:- begin_tests(rdf_e).
% Moved to utils.pl
:- end_tests(rdf_e).


rdf_e(S,P,O) :-
        builtin(P,Pred),
        get_list(S,SS),
        get_list(O,OO),
        catch(call_builtin(P,Pred,SS,OO),_,fail).
rdf_e(S,P,O) :-
	pl_list_to_rdf_list(S,Q1T,SS), list_to_conj(Q1T,Q1),
	pl_list_to_rdf_list(O,Q2T,OO), list_to_conj(Q2T,Q2),
	rdf_db:(Q1,Q2,rdf(SS,P,OO,G)),G\=_:_,
	\+rdf_db:rdf(G,_,_),\+rdf_db:rdf(_,_,G),P\='http://www.w3.org/2000/10/swap/log#implies'.

% Use rdf_reachable for handling owl:sameAs

call_builtin(P,Pred,SS,OO) :-
	\+tabled(P),
	call(Pred,SS,OO).
call_builtin(P,Pred,SS,OO) :-
	tabled(P), \+cache(SS,P,OO),!,
	call(Pred,SS,OO),
	term_to_atom(cache(SS,P,OO),T),atom_to_term(T,ToAssert,_), % FIXME - loss of precision
	assert(ToAssert).
	%rdf_assert_g(SSS,SS,G1),
	%rdf_assert_g(OOO,OO,G2),
	%literal_to_node(SSS,SSSS),
	%rdf_db:(rdf_assert(SSSS,P,OOO),G1,G2).
call_builtin(P,_,SS,OO) :-
	tabled(P), writeln('looking up in cache'),
	cache(SS,P,OO),
	mem_load(SS),mem_load(OO). % FIXME - should be in the builtin - all builtins should have their metadata cached anyway

:- dynamic cache/3.
cache(S,P,O) :-
	rdf_db:rdf(SS,P,OO,cache),
	%literal_to_node(SSS,SS),
	P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#first',P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
	get_list(SS,S),get_list(OO,O).
	

tabled(P) :-
	rdf_db:rdf(P,rdf:type,'http://purl.org/ontology/tabling/TabledPredicate').


