#!/usr/local/bin/pl -q -s 

% Just a small script, equivalent
% of cwm $1 --think --rdf --data
% 
% Syntax: swicwm.pl <path_to_n3_file>
%
% A couple of examples are available
% in examples/.

:- use_module(library(rdf)).
:- [n3_load].
:- [n3_to_prolog].
:- [entailment].



think(File) :-
	n3_load:n3_load(File),
	n3_to_prolog:compile_all,
	findall(rdf(S,P,O),entailment:rdf(S,P,O),Bag),
	rdf_write_xml(user_output,Bag).

:- 
	current_prolog_flag(argv,L),
	last(L,File),
	think(File),
	halt.


