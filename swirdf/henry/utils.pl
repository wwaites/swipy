:- module(utils,[list_to_conj/2,literal_to_node/2,pl_list_to_rdf_list/3,get_list/2]).

:- begin_tests(utils).
:- use_module(library('semweb/rdf_db')).
test(list_conversion) :-
	rdf_db:(rdf_assert(l1,rdf:first,a),rdf_assert(l1,rdf:rest,l2),rdf_assert(l2,rdf:first,l3),rdf_assert(l2,rdf:rest,rdf:nil),rdf_assert(l3,rdf:first,b),rdf_assert(l3,rdf:rest,l4),rdf_assert(l4,rdf:first,c),rdf_assert(l4,rdf:rest,rdf:nil)),
	get_list(l1,[a,[b,c]]),!,
	pl_list_to_rdf_list([a,[b,c]],E,_),list_to_conj(E,Goal),!,
	rdf_db:Goal,!,
	rdf_db:rdf_reset_db.
:- end_tests(utils).


list_to_conj([],true) :- !.
list_to_conj([H],H) :- !.
list_to_conj([H,T],(H,T)) :- !.
list_to_conj([H|T],(H,T2)) :-
        list_to_conj(T,T2).

% subject as literals hack...
literal_to_node(Literal,Node) :- var(Literal), atom_concat('__literal',_,Node),atom_concat('__',At,Node),term_to_atom(Literal,At),!.
literal_to_node(At,At) :- atomic(At).
literal_to_node(literal(Lit),Node) :- var(Node), term_to_atom(literal(Lit),At), atom_concat('__',At,Node).

pl_list_to_rdf_list(N,[],N) :- \+is_list(N),(atomic(N);var(N);(compound(N),N=literal(_))),!.
pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')|Triples],H2) :- is_list(H),pl_list_to_rdf_list(H,Triples,H3),!.
pl_list_to_rdf_list([H],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest','http://www.w3.org/1999/02/22-rdf-syntax-ns#nil')],H2) :-  !.
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H3),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
        is_list(H),!,
        pl_list_to_rdf_list(H,Triples1,H3),
        pl_list_to_rdf_list(T,Triples2,T2),
        append(Triples1,Triples2,Triples).
pl_list_to_rdf_list([H|T],[rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H),rdf(H2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',T2)|Triples],H2) :-
        pl_list_to_rdf_list(T,Triples,T2).

get_list(S,[]) :- S =='http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',!.
get_list(S,[H|T]) :-
        atomic(S),
	rdf_db:rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',H2),
        get_list(H2,H),
        rdf_db:rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',S2),
        !,get_list(S2,T).
get_list(A,A).


