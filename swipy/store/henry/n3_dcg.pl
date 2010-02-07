:- module(n3_dcg,[document/4,tokenise/2]).

/**
 * This module implements a DCG to parse N3 in SWI.
 * The parsing is done in two steps. First
 * the N3 file is tokenised (using a tokeniser
 * similar to the turtle one available in SWI' rdf_turtle.pl).
 * Then, a DCG gives back a quad representation of the parsed
 * N3. 
 *
 * Basically, each quad is of the form rdf(Subject,Predicate,Object,Context).
 * Existentially quantified variables are modeled as blank nodes (eg. __bnode* atoms).
 * Universally quantified variables are also modeled as blank nodes, but of the form
 * __bnode_*_uqvar.
 *
 * This is a nasty hack, but it allows to use the really efficient SWI RDF
 * store for storage of N3 formulae :-)
 * 
 * In the same directory, a test suite is available (taken from CWM CVS 
 * repository), as well as the scripts allowing to run it and the results
 * as of now.
 *
 * TODO (check):
 *   * @forAll?
 *
 * Copyright Yves Raimond (c) 2007
 * Centre for Digital Music, Queen Mary, University of London
 */


:- op(100,xfx,':').
:- op(100,fx,':').

:- use_module(library('semweb/rdf_db')).

tokenise(File,Tokens) :-
        open(File,read,S),
        grab_tokens(S,Tokens).

grab_tokens(S,Tokens) :-
        turtle_tokens(S,Tokens).

graph_id(GraphID) :-
	gensym('__bnode_graph_',GraphID).

uqv_id(Name,UQV) :-
	format(atom(UQV),'__bnode_~w_uqvar',[Name]).


:- dynamic base_uri/1.
:- dynamic ns/2.
%in the n3.n3 grammar, doesn't "zeroOrMore" overlaps with
%() or ...?
document(BaseURI,Document) -->
	{retractall(base_uri(_)),assert(base_uri(BaseURI))},
	statements_optional(BaseURI,Document).

statements_optional(BaseURI,Triples) -->
	statement(BaseURI,Tr),{append(Tr,T,Triples)},
	['.'],!,
	statements_optional(BaseURI,T).
statements_optional(_,[]) --> [].

formulacontent(_BaseURI,GraphURI,Formula) -->
	{graph_id(GraphURI)},
	statementlist(GraphURI,Formula).


statementlist(BaseURI,Triples) -->
	statement(BaseURI,Tr),{append(Tr,T,Triples)},!,
	statementtail(BaseURI,T).
statementlist(_,[]) --> [].

statement(_Base,[D]) --> 
	declaration(D).
statement(_Base,[U]) -->
	universal(U).
statement(_Base,[E]) --> 
	existential(E).
statement(BaseURI,Statement) -->
	simpleStatement(BaseURI,Statement).

statementtail(Base,T) -->
	['.'],!,
	statementlist(Base,T).
statementtail(_,[]) --> [].

universal(universal(Symbols)) --> 
	['@',name(forAll)],!,
	csl_symbol(Symbols).

existential(existential(Symbols)) --> 
	['@',name(forSome)],!,
	csl_symbol(Symbols).

declaration(namespace(Prefix,URI)) -->
	['@',name(prefix)],
	prefix(Prefix),[':'],!,
	explicituri(URI),{assert(ns(Prefix,URI))}.
declaration(namespace(base,URI)) -->
	['@',name(prefix),':'],!,
	explicituri(URI),{retractall(base_uri(_)),assert(base_uri(URI))}.
declaration(namespace(base,URI)) -->
	['@',name(base)],!,
	explicituri(URI),{retractall(base_uri(_)),assert(base_uri(URI))}.

declaration(keywords(List)) -->
	['@',name(keywords)],!,
	csl_barename(List).

simpleStatement(Base,Triples) -->
	subject(Base,Subject,Triples1),
	propertylist(Base,Subject,Triples2),
	{append(Triples1,Triples2,Triples)}.

propertylist(Base,Subject,[rdf(Subject,Verb,Object,Base)|Triples]) -->
	verb(Base,Verb,Triples1),
	!,
	object(Base,Object,Triples2),
	objecttail(Base,Subject,Verb,Triples3),
	propertylisttail(Base,Subject,Triples4),
	{append(Triples1,Triples2,Triples12),
	 append(Triples12,Triples3,Triples123),
	 append(Triples123,Triples4,Triples)}.
propertylist(_Base,_,[]) --> [].

propertylisttail(Base,Subject,Triples) -->
	[';'],!,
	propertylist(Base,Subject,Triples).
propertylisttail(_Base,_,[]) --> [].

objecttail(Base,Subject,Verb,[rdf(Subject,Verb,Node,Base)|T]) --> 
	[','],!,
	object(Base,Node,Triples),
	{append(Triples,Tail,T)},
	objecttail(Base,Subject,Verb,Tail).
objecttail(_Base,_,_,[]) --> [].

verb(Base,Node,Triples) -->
	['@',name(has)],!,
	path(Base,Node,Triples).
%i am not sure if it is ok without the @, but cwm seems to 
%handle this
verb(Base,Node,Triples) --> 
	[name(has)],!,
	path(Base,Node,Triples).
verb(Base,BNode,[rdf(BNode,'http://www.w3.org/2002/07/owl#inverseOf',Node,Base)|Triples]) -->
	['@',name('is')],
	path(Base,Node,Triples),
	['@,',name(of)],!,
	{rdf_bnode(BNode)}.
verb(Base,BNode,[rdf(BNode,'http://www.w3.org/2002/07/owl#inverseOf',Node,Base)|Triples]) --> 
	[name('is')],
	path(Base,Node,Triples),
	[name(of)],!,
	{rdf_bnode(BNode)}.
verb(_Base,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',[]) -->
	['@',name(a)],!.
verb(_Base,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',[]) -->
	[name(a)],!.
verb(_Base,'http://www.w3.org/2000/10/swap/log#implies',[]) -->
        ['=','>'],!. %this order *is* important:)
verb(_Base,'http://www.w3.org/2002/07/owl#sameAs',[]) -->
	['='],!.
verb(Base,BNode,[rdf(BNode,'http://www.w3.org/2002/07/owl#inverseOf','http://www.w3.org/2000/10/swap/log#implies',Base)]) -->
	['<','='],!,{rdf_bnode(BNode)}.
verb(Base,Node,Triples) -->
        path(Base,Node,Triples).

subject(Base,Node,Triples) --> 
	path(Base,Node,Triples).

object(Base,Node,Triples) -->
	path(Base,Node,Triples).

path(Base,Node,[rdf(N1,N2,BNode,Base)|Triples]) -->
	node(Base,N1,Triples1),['!'],!,
	node(Base,N2,Triples2), {rdf_bnode(BNode)},
	{append(Triples1,Triples2,Triples12),
	append(Triples12,Tail,Triples)},
	path(Base,Node,BNode,Tail).
path(Base,Node,[rdf(BNode,N2,N1,Base)|Triples]) -->
	node(Base,N1,Triples1),['^'],!,
	node(Base,N2,Triples2), {rdf_bnode(BNode)},
	{append(Triples1,Triples2,Triples12),
	 append(Triples12,Tail,Triples)},
	path(Base,Node,BNode,Tail).
path(Base,Node,Triples) --> node(Base,Node,Triples).
path(Base,Node,BNode,[rdf(BNode,N2,BNode2,Base)|Triples]) -->
	['!'],!,
	node(Base,N2,Triples2), {rdf_bnode(BNode2)},
	{append(Triples2,Tail,Triples)},
	path(Base,Node,BNode2,Tail).
path(Base,Node,BNode,[rdf(BNode2,N2,BNode,Base)|Triples]) -->
	['^'],!,
	node(Base,N2,Triples2), {rdf_bnode(BNode2)},
	{append(Triples2,Tail,Triples)},
	path(Base,Node,BNode2,Tail).
path(_Base,Node,Node,[]) --> [].


node(Base,Symbol,[]) -->
	symbol(S),{S=(:A)},!,{base_uri(URI)->atom_concat(URI,A,Symbol);atom_concat(Base,A,Symbol)}.
node(_Base,Symbol,[]) -->
	symbol(Symbol),!.
node(_Base,VarID,[]) -->
	['?'],!,variable(Variable),
	{uqv_id(Variable,VarID)}.
node(_Base,literal(Number),[]) -->
	numericliteral(Number),!.
node(_Base,literal(Literal),[]) --> 
	literal(Literal),!.
node(_Base,literal(Boolean),[]) -->
	boolean(Boolean),!.
node(Base,BNode,Triples) -->
	['['],!,{rdf_bnode(BNode)},
	propertylist(Base,BNode,Triples),
	[']'].
node(Base,List,Triples) --> 
	['('],!,
	pathlist(Base,List,Triples),
	[')'].
node(Base,Node,Triples) -->
        ['{'],!,
        formulacontent(Base,Node,Triples),
        ['}'].

%node -->
%	['@this']. %deprecated

pathlist(Base,BNode,[rdf(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List',Base),rdf(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',Node,Base),rdf(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',Rest,Base)|Triples]) --> 
	path(Base,Node,T),!,pathlist(Base,Rest,Tail),{rdf_bnode(BNode)},
	{append(T,Tail,Triples)}.
pathlist(_Base,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) --> [].

symbol(URI) --> 
	explicituri(URI).
symbol(QName) -->
	qname(QName).
symbol(BNode) -->
	bnode(BN),
	{atom_concat('__bnode_',BN,BNode)}.

%dtlang(Lang) -->
%	['@'],!,langcode(Lang).
%dtlang(Datatype) -->
%	['^','^'],!,symbol(Datatype).
%dtlang --> [].

/**
 * Coma separated period terminated list grammar
 */
csl_symbol([Symbol|T]) -->
	symbol(Symbol),
	[','],!,
	csl_symbol(T).
csl_symbol([Symbol]) -->
	symbol(Symbol).
csl_barename([BareName|T]) -->
	barename(BareName),
	[','],!,
	csl_barename(T).
csl_barename([BareName]) -->
	barename(BareName).


/**
 * TERMINALS
 */

boolean(true) -->
	['@',name('true')].
boolean(fail) -->
	['@',name('false')].

numericliteral(Num) -->
	[numeric(_,NumC)],
	{number_codes(Num,NumC)}.
	%{matches(NumericLiteral,'[-+]?[0-9]+(\\.[0-9]+)?(e[-+]?[0-9]+)?')}.

explicituri(ExplicitURI) -->
	[relative_uri(ExplicitURI)],
	{matches(ExplicitURI,'[^>]*')}.

prefix(Prefix) -->
	[name(Prefix)]. %should add a regexp match

qname(URI) --> [':'],{base_uri(URI)},!.
qname(URI) -->
	[':'(NS,Name)],{ns(NS,Base),atom_concat(Base,Name,URI)},!.
	%{ matches(Name,'([a-zA-Z_][a-zA-Z0-9_]*)?'),
	%  matches(NS,'([a-zA-Z_][a-zA-Z0-9_]*)?')
	%}.
qname(URI) -->
	[':'(Name)],{base_uri(Base),atom_concat(Base,Name,URI)}.
	%{matches(_Name,'([a-zA-Z_][a-zA-Z0-9_]*)?')}.
bnode(BNode) -->
	[nodeId(BNode)].

barename(BareName) -->
	[name(BareName)],
	{matches(BareName,'[a-zA-Z_][a-zA-Z0-9_]*')}.

variable(Variable) -->
	[name(Variable)],
	{matches(Variable,'[a-zA-Z_][a-zA-Z0-9_]*')}.

%langcode(Langcode) -->
%	[Langcode],
%	{matches(Langcode,'[a-z]+(-[a-z0-9]+)*')}.

literal(Literal) -->
	[literal(Literal)]. %FALSE REGEXP
	%{matches(String,'(\"\"\"[^\"\\\\]*(?:(?:\\\\.|\"(?!\"\"))[^\"\\\\]*)*\"\"\")|(\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\")')}.

/**
 * RegExp match
 */
matches(Atom,RegEx) :-
	new(S,string(Atom)),
	new(R,regex(string(RegEx))),
	send(R,match,S).


/**
 * Parsing exception handling
 */
grammar_error(Which) :- 
        throw(error(grammar_error(Which))).


/**
 * Tokenizer - stolen from rdf_turtle.pl
 */

turtle_tokens(In, List) :-
	get_code(In, C0),
	turtle_token(C0, In, C1, Tok1), 
	(   Tok1 == end_of_file
	->  List = end_of_file
	;   List = [Tok1|Tokens],
	    turtle_tokens(C1, In, Tokens)
	).

turtle_tokens(C0, In, List) :-
	(   turtle_token(C0, In, C1, H)
	->  debug(turtle(token), 'Token: ~q', [H])
	;   syntax_error(In, -1, illegal_token)
	),
	(   
	H == end_of_file
	->  List=[]
	;   List = [H|T],
	    turtle_tokens(C1, In, T)
	).

turtle_token(-1, _, -1, end_of_file) :- !.
turtle_token(0'., In, C, '.') :- !,
	get_code(In, C).
turtle_token(0'#, In, C, Token) :- !,
	get_code(In, C1),
	skip_line(C1, In, C2),
	turtle_token(C2, In, C, Token).
turtle_token(WS, In, C, Token) :-
	turtle_ws(WS), !,
	get_code(In, C1),
	turtle_token(C1, In, C, Token).
turtle_token(C0, In, C, Number) :-
	between(0'0, 0'9, C0), !,
	turtle_number(C0, In, C, Number).
turtle_token(0'-, In, C, Number) :- !,
	turtle_number(0'-, In, C, Number).
turtle_token(0'+, In, C, Number) :- !,
	turtle_number(0'+, In, C, Number).
turtle_token(0'", In, C, Literal) :- !,
	(peek_code(In,34) ->
		(get_code(In,34),get_code(In,34),
		get_code(In,C1),
		turtle_dq_string(C1, In, C2, Codes))
		;
		(get_code(In, C1),
		turtle_string(C1, In, C2, Codes))
		),
	atom_codes(Atom, Codes),
	(   C2 == 0'@
	->  get_code(In, C3),
	    language(C3, In, C, LangCodes),
	    atom_codes(LangId, LangCodes),
	    Literal = literal(lang(LangId, Atom))
	;   C2 == 0'^,
	    peek_code(In, 0'^)
	->  get_code(In, 0'^),
	    get_code(In, C3),
	    resource_token(C3, In, C, Type),
	    Literal = literal(type(Type, Atom))
	;   C = C2,
	    Literal = literal(Atom)
	).
turtle_token(0'_, In, C, nodeId(NodeID)) :-
	peek_code(In, 0':), !,
	get_code(In, _),
	get_code(In, C1),
	name(C1, In, C, NodeID).
turtle_token(0'<, In, C, URI) :- 
	peek_code(In, C1),C1\=61,!, %do not match <=
	resource_token(0'<, In, C, URI),!.
turtle_token(0':, In, C, URI) :- !,
	resource_token(0':, In, C, URI).
turtle_token(C0, In, C, Token) :-
	name(C0, In, C1, Name), !,
	(   C1 == 0':,
	    \+ sub_atom(Name, 0, _, _, '_'),
	    peek_code(In, C2),
	    name_start_char(C2)
	->  get_code(In, C2),
	    name(C2, In, C, Name2),
	    Token = (Name:Name2)
	;   Token = name(Name),
	    C = C1
	).
turtle_token(Punct, In, C, P) :-
	punctuation(Punct, P), !,
	get_code(In, C).

%%	turtle_number(+Char0, +In, -CharNext, -Value)
%	
%	Value is Type:CodeList

turtle_number(0'-, In, CN, numeric(T, [0'-|Codes])) :- !,
	get_code(In, C0),
	turtle_number_nn(C0, In, CN, numeric(T, Codes)).
turtle_number(0'+, In, CN, numeric(T, [0'+|Codes])) :- !,
	get_code(In, C0),
	turtle_number_nn(C0, In, CN, numeric(T, Codes)).
turtle_number(C0, In, CN, Value) :-
	turtle_number_nn(C0, In, CN, Value).

turtle_number_nn(C, In, CN, numeric(Type, Codes)) :-
	turtle_integer_codes(C, In, CN0, Codes, T0), 	% [0-9]+ 
	(   CN0 == 0'.
	->  T0 = [CN0|T1],
	    get_code(In, C1),
	    turtle_integer_codes(C1, In, CN1, T1, T2), % [0-9]+.[0-9]+
	    (	exponent(CN1, In, CN, T2)
	    ->	Type = double
	    ;	CN = CN1,
		T2 = [],
		Type = decimal
	    )
	;   exponent(CN0, In, CN, T0)
	->  Type = double
	;   T0 = [],
	    CN = CN0,
	    Type = integer
	).

turtle_integer_codes(C0, In, CN, [C0|T0], T) :-
	between(0'0, 0'9, C0), !,
	get_code(In, C1),
	turtle_integer_codes(C1, In, CN, T0, T).
turtle_integer_codes(CN, _, CN, T, T).

exponent(C0, In, CN, [C0|T0]) :-
	e(C0), !,
	get_code(In, C1),
	optional_sign(C1, In, CN0, T0, T1),
	turtle_integer_codes(CN0, In, CN, T1, []).

optional_sign(C0, In, CN, [C0|T], T) :-
	sign(C0), !,
	get_code(In, CN).
optional_sign(CN, _, CN, T, T).

e(0'e).
e(0'E).

sign(0'-).
sign(0'+).

turtle_dq_string(-1, In, _, []) :- !,
	syntax_error(In, -1, unexpected_end_of_input).
turtle_dq_string(0'", In, C, []) :- 
	get_code(In, 34),get_code(In,34),!,get_code(In,C).
turtle_dq_string(0'\\, In, C, [H|T]) :- 
	get_code(In, C1),
	string_escape(C1, In, C2, H),!,writeln(C2),
	turtle_dq_string(C2, In, C, T).
turtle_dq_string(C0, In, C, [C0|T]) :-
	get_code(In, C1),
        turtle_dq_string(C1, In, C, T).

					% string
turtle_string(-1, In, _, []) :- !,
	syntax_error(In, -1, unexpected_end_of_input).
turtle_string(0'", In, C, []) :- !,
	get_code(In, C).
turtle_string(0'\\, In, C, [H|T]) :- 
	get_code(In, C1),
	string_escape(C1, In, C2, H),!,
	turtle_string(C2, In, C, T).
turtle_string(C0, In, C, [C0|T]) :-
	get_code(In, C1),
	turtle_string(C1, In, C, T).


string_escape(0'n, In, C, 0'\n) :- !,
	get_code(In, C).
string_escape(0'", In, C, 0'") :- !,
	get_code(In, C).
string_escape(0'\\, In, C, 0'\\) :- !,
	get_code(In, C).
string_escape(0't, In, C, 0'\t) :- !,
	get_code(In, C).
string_escape(0'r, In, C, 0'\r) :- !,
	get_code(In, C).
string_escape(0'u, In, C, Code) :- !,
	get_hhhh(In, Code),
	get_code(In, C).
string_escape(0'U, In, C, Code) :- !,
	get_hhhh(In, Code0),
	get_hhhh(In, Code1),
	Code is Code0 << 16 + Code1,
	get_code(In, C).

get_hhhh(In, Code) :-
	get_code(In, C1), code_type(C1, xdigit(D1)),
	get_code(In, C2), code_type(C2, xdigit(D2)),
	get_code(In, C3), code_type(C3, xdigit(D3)),
	get_code(In, C4), code_type(C4, xdigit(D4)),
	Code is D1<<12+D2<<8+D3<<4+D4.

					% language: [a-z]+ ('-' [a-z0-9]+ )*
language(C0, In, C, [C0|Codes]) :-
	code_type(C0, lower),
	get_code(In, C1),
	lwr_word(C1, In, C2, Codes, Tail),
	sub_langs(C2, In, C, Tail, []).

lwr_word(C0, In, C, [C0|T0], T) :-
	code_type(C0, lower), !,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwr_word(C, _, C, T, T).

sub_langs(0'-, In, C, [0'-, C1|Codes], T) :- !,
	get_code(In, C1),
	lwrdig(C1), !,
	get_code(In, C2),
	lwrdigs(C2, In, C3, Codes, Tail),
	sub_langs(C3, In, C, Tail, T).
sub_langs(C, _, C, T, T).

lwrdig(C) :-
	code_type(C, lower), !.
lwrdig(C) :-
	code_type(C, digit).

lwrdigs(C0, In, C, [C0|T0], T) :-
	lwrdig(C0), !,
	get_code(In, C1),
	lwr_word(C1, In, C, T0, T).
lwrdigs(C, _, C, T, T).

					% resource_token
resource_token(0'<, In, C, relative_uri(URI)) :- 
	get_code(In,C1),
	uri_chars(C1, In, C, Codes),
	atom_codes(URI, Codes).
resource_token(0':, In, C, Token) :- !,
	get_code(In, C0),
	(   name(C0, In, C, Name)
	->  Token = :(Name)
	;   Token = :,
	    C = C0
	).
resource_token(C0, In, C, Prefix:Name) :-
	name(C0, In, C1, Prefix),
	\+ sub_atom(Prefix, 0, _, _, '_'), !,
	C1 == 0':,
	get_code(In, C2),
	name(C2, In, C, Name).


%%	uri_chars(+Char, +In:stream, -NextChar, -UriChars) is semidet.

uri_chars(0'>, In, C, []) :- !,
	get_code(In, C).
uri_chars(0'\\, In, C, [H|T]) :- !,
	get_code(In, C1),
	string_escape(C1, In, C2, H),
	uri_chars(C2, In, C, T).
uri_chars(-1, _, _, _) :- !,
	fail.
uri_chars(C0, In, C, [C0|T]) :-
	get_code(In, C1),
	uri_chars(C1, In, C, T).

					% name
name(C0, In, C, Atom) :-
	name_start_char(C0),
	get_code(In, C1),
	name_chars(C1, In, C, T),
	atom_codes(Atom, [C0|T]).

name_chars(C0, In, C, [C0|T]) :-
	name_char(C0), !,
	get_code(In, C1),
	name_chars(C1, In, C, T).
name_chars(C, _, C, []).

name_start_char(C) :- code_type(C, csymf).
name_start_char(C) :- between(0xC0, 0xD6, C).
name_start_char(C) :- between(0xD8, 0xF6, C).
name_start_char(C) :- between(0xF8, 0x2FF, C).
name_start_char(C) :- between(0x370, 0x37D, C).
name_start_char(C) :- between(0x37F, 0x1FFF, C).
name_start_char(C) :- between(0x200C, 0x200D, C).
name_start_char(C) :- between(0x2070, 0x218F, C).
name_start_char(C) :- between(0x2C00, 0x2FEF, C).
name_start_char(C) :- between(0x3001, 0xD7FF, C).
name_start_char(C) :- between(0xF900, 0xFDCF, C).
name_start_char(C) :- between(0xFDF0, 0xFFFD, C).
name_start_char(C) :- between(0x10000, 0xEFFFF, C).

name_char(C) :-	name_start_char(C).
name_char(0'-).
name_char(D) :-	code_type(D, digit).
name_char(0xB7).
name_char(C) :- between(0x0300, 0x036F, C).
name_char(C) :- between(0x203F, 0x2040, C).

punctuation(0'(, '(').
punctuation(0'), ')').
punctuation(0'[, '[').
punctuation(0'], ']').
punctuation(0',, ',').
punctuation(0'@, '@').
punctuation(0':, ':').
punctuation(0';, ';').
punctuation(0'{, '{').
punctuation(0'}, '}').
punctuation(0'?,'?').
punctuation(0'!,'!').
punctuation(0'^,'^').	
punctuation(0'=,'=').
punctuation(0'<,'<').
punctuation(0'>,'>').

skip_line(0xA, In, C) :- !,
	get_code(In, C).
skip_line(0xD, In, C) :- !,
	get_code(In, C).
skip_line(_, In, C) :- !,
	get_code(In, C1),
	skip_line(C1, In, C).

					% ws
turtle_ws(0x9).
turtle_ws(0xA).
turtle_ws(0xD).
turtle_ws(0x20).

syntax_error(Stream, -1, Which) :- !,
        stream_property(Stream, file_name(File)),
        line_count(Stream, LineNo),
        line_position(Stream, LinePos),
        character_count(Stream, CharIndex),
        throw(error(syntax_error(Which),
                    file(File, LineNo, LinePos, CharIndex))).
syntax_error(Stream, LineNo, Which) :-
        stream_property(Stream, file_name(File)),
        throw(error(syntax_error(Which),
                    file(File, LineNo, -1, -1))).


