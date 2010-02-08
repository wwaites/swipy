:- module(namespaces,[]).

:- use_module(library('semweb/rdf_db')).

:- rdf_register_ns(pl,'http://purl.org/ontology/swipl/').
:- rdf_register_ns(log,'http://www.w3.org/2000/10/swap/log#').
:- rdf_register_ns(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_ns(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_ns(owl,'http://www.w3.org/2002/07/owl#').
:- rdf_register_ns(list,'http://www.w3.org/2000/10/swap/list#').
:- rdf_register_ns(af,'http://purl.org/ontology/af/').
:- rdf_register_ns(vamp,'http://purl.org/ontology/vamp/').
:- rdf_register_ns(mo,'http://purl.org/ontology/mo/').
:- rdf_register_ns(to,'http://purl.org/ontology/tonality/').
:- rdf_register_ns(tl,'http://purl.org/NET/c4dm/timeline.owl#').
:- rdf_register_ns(event,'http://purl.org/NET/c4dm/event.owl#').
:- rdf_register_ns(t,'http://purl.org/ontology/tabling/').

