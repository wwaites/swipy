:- module(builtins,[builtin/2]).


/**
 * This modules provides anchors for builtin predicates.
 * By registering a new builtin(Predicate,CorrespondingPrologPredicate)
 * clause, proving Predicate using the n3_entailment
 * module will call CorrespondingPrologPredicate.
 *
 * The file examples/list.n3 gives an illustration of that, 
 * and uses the list:member/2 SWI predicate.
 *
 * Copyright Yves Raimond (c) 2007
 * Centre for Digital Music, Queen Mary, University of London
 */


:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).

:- rdf_meta builtin(r,?).
:- multifile builtin/2.


/**
 *
 * Declaration of builtin predicates
 *
 */

builtin('http://www.w3.org/2000/10/swap/list#in',builtin:member).
builtin('http://www.w3.org/2000/10/swap/list#nextto',builtin:nextto).
 % Should be builtin(A,B) :- ... for namespace expansion

/**
 * Each builtin is a binary predicate - first argument is the 
 * argument or the list of arguments used as a subject, second argument
 * is the argument or the list of arguments used as an object
 */
builtin:member(A,B) :- 
	\+var(B),
	member(A,B).
builtin:nextto([X,Y],L) :-
	\+var(L),
	getnext(X, Y, L).

getnext(X, Y, L) :-
	nextto(X,Y,L).
getnext(X, X, L) :-
 	\+var(L),
	last(L, X).



%Just illustrating a bug
%builtin('http://purl.org/ontology/swi/append',builtin:append).
%builtin:append([[a,b],[c]],[a,b,c]).

