:- module(n3_to_prolog,[n3_prolog_clause/1,compile_all/0]).

:- use_module(skolemize).
:- use_module(utils).
:- consult(namespaces).

:- use_module(library('semweb/rdf_db'),
              [ rdf_global_id/2,
                rdf_reachable/3,
                rdf_has/3,
                rdf_bnode/1,
                rdf_is_bnode/1,
                rdf_subject/1,
                rdf_equal/2
              ]).
:- use_module(library('semweb/rdfs'),
              [ rdfs_list_to_prolog_list/2 ]).


/**
 * Some unit tests
 */
:- begin_tests(n3_to_prolog).
:- end_tests(n3_to_prolog).
/**
 *
 */


compile_all :-
	forall(n3_prolog_clause(Clause),(format(user_error,'~w\n\n',[Clause]),assert(rdf_e:Clause))).

n3_prolog_clause(':-'(H,B)) :-
	implies(BodyGraph,HeadGraph),
	n3_pl(head,HeadGraph,PLH,B1),
	n3_pl(body,BodyGraph,PLB,B2),
	append(B1,B2,Bindings),
	merge_bindings(Bindings),
	skolemize(PLH,PLB,Skolem),
	member(H,Skolem),
	list_to_conj(PLB,B).

n3_pl(T,Head,PredList,Bindings) :-
        findall(rdf_e(S,P,O),rdf_db:rdf(S,P,O,Head),Triples),
        n3_pl2(T,Triples,PredList,Bindings).
n3_pl2(_,[],[],[]).
n3_pl2(HB,[rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',_)|T],T2,B) :-
        !,n3_pl2(HB,T,T2,B).
n3_pl2(HB,[rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',_)|T],T2,B) :-
        !,n3_pl2(HB,T,T2,B).
n3_pl2(HB,[rdf_e(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List')|T],T2,B) :-
        !,n3_pl2(HB,T,T2,B).
n3_pl2(HB,[rdf_e(S,P,O)|T],[rdf_e(SS,PP,OO)|T2],Bindings) :-
        convert_n(HB,S,SS,B1),
        convert_n(HB,P,PP,B2),
        convert_n(HB,O,OO,B3),
        flatten([B1,B2,B3],BH),
        n3_pl2(HB,T,T2,BT),
        append(BH,BT,Bindings).

convert_all(_,[],[],[]).
convert_all(HB,[H|T],[H2|T2],Bindings) :-
        convert_n(HB,H,H2,BH),
        convert_all(HB,T,T2,BT),
        append(BH,BT,Bindings).
convert_n(HB,S,SL,Bindings) :-
        atomic(S),
        rdfs_list_to_prolog_list(S,SLT),!,
        convert_all(HB,SLT,SL,Bindings).
convert_n(_,S,T,[binding(S,T)]) :-
        (universal(S)),!.
convert_n(head,S,bnode(T),[binding(S,T)]) :-
        existential(S),!.
convert_n(body,S,T,[binding(S,T)]) :-
        existential(S),!.
convert_n(_,S,S,[]).


merge_bindings([]).
merge_bindings([binding(Node,Term)|T]) :-
        associate(binding(Node,Term),T),
        merge_bindings(T).
associate(_,[]).
associate(binding(Node,Term),[binding(Node,Term)|T]) :-
        !,
        associate(binding(Node,Term),T).
associate(binding(Node,Term),[_|T]) :-
        associate(binding(Node,Term),T).

/**
 * Check the quantification of a variable
 */
existential(Node) :-
        quantification(Node,Q),
        Q=existential.
universal(Node) :-
        quantification(Node,Q),
        Q=universal.
quantification(Node,universal) :-
        Node\=[_|_],
        rdf_db:rdf_is_bnode(Node),
        atom_concat(_,'_uqvar',Node),!.
quantification(Node,existential) :-
        Node\=[_|_],
        rdf_db:rdf_is_bnode(Node).

/**
 * Get back two named graph identifiers linked
 * together through log:implies
 */
implies(Body,Head) :-
        rdf_db:rdf(Body,log:implies,Head).

