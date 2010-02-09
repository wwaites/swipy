#!/usr/local/bin/pl -g serql_welcome -L128m -G128m -T128m -s

:- load_files([ 'SeRQL/load'
              ],
              [ silent(true)
              ]).

:- use_module(n3_entailment).

:- [n3_load].
:- [n3_to_prolog].
:- [n3_entailment].

server :-
        serql_server([]).

server(Port) :-
        serql_server([port(Port)]).

:- server(1212).


