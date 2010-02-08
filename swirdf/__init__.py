from rdflib.graph import Graph
from rdflib.store import Store
from rdflib.plugin import register
from rdflib.term import URIRef, Literal, BNode
from swi import *
import os

__all__ = ["SWIStore"]

##
## :- dynamic user:file_search_path/2
## :- multifile user:file_search_path/2
##
file_search_path = Functor("file_search_path", 2)
_fsp = colon(Atom("user"), file_search_path)
call(assertz(clause([], [dynamic(_fsp)])))
call(assertz(clause([], [multifile(_fsp)])))

##
## Set up paths, similar to SeRQL/load.pl
##
basedir = os.path.dirname(__file__)
def set_path(name, relpath):
	path = os.path.join(basedir, relpath)
	call(assertz(file_search_path(Atom(name), Atom(path))), module="user")

set_path("serql", "SeRQL")
set_path("library", "SeRQL/lib")
set_path("henry", "henry")
serql = Functor("serql")
henry = Functor("henry")

##
## Read in Prolog Dependencies
##
call(load_files([
	library(Atom("semweb/rdf_db")),
	library(Atom("semweb/rdf_persistency")),
	library(Atom("semweb/rdf_portray")),
#	serql(Atom("sparql")),
#	serql(Atom("no_entailment")),
#	serql(Atom("rdf_entailment")),
#	serql(Atom("rdfs_entailment")),
#	serql(Atom("rdfslite_entailment")),
	serql(Atom("load")),
	henry(Atom("n3_load")),
#	henry(Atom("n3_entailment"))
]))

##
## Functions from rdf_db
##
rdf_load = Functor("rdf_load", 2)
rdf_unload = Functor("rdf_unload")
rdf = Functor("rdf", 3)
rdf4 = Functor("rdf", 4)
rdf_assert = Functor("rdf_assert", 3)
rdf_assert4 = Functor("rdf_assert", 4)
rdf_retractall = Functor("rdf_retractall", 3)
rdf_retractall4 = Functor("rdf_retractall", 4)
rdf_transaction = Functor("rdf_transaction", 2)

##
## Functors relating to persistency
##
rdf_attach_db = Functor("rdf_attach_db", 2)
rdf_detach_db = Functor("rdf_detach_db", 0)
rdf_current_db = Functor("rdf_current_db")

##
## Terms used throughout
##
literal = Functor("literal")
lang = Functor("lang", 2)
dtype = Functor("type", 2)
graph = Functor("graph")
db = Functor("db")
colon = Functor(":", 2)
log = Functor("log")
message = Functor("message")

sparql_query = Functor("sparql_query", 3)
entailment = Functor("entailment")
entailment2 = Functor("entailment", 2)

####
#### SWI-Prolog backed RDFLib Store
####
class SWIStore(Store):
	context_aware = True
	def __init__(self):
		self.index = 0
		self.entailment = None
	def open(self, directory, create=False):
		call(rdf_attach_db(Atom(directory), []), module="rdf_persistency")
	def close(self, commit_pending_transaction=False):
		if self.attached:
			call(rdf_detach_db(), module="rdf_persistency")
	def attached(self):
		frame = Frame()
		X = Variable()
		backing = [X.value for x in Query(rdf_current_db(X), module="rdf_persistency")]
		if backing: result = backing[0]
		else: result = None
		frame.discard()
		return result
	attached = property(attached)
	def add(self, statement, context=None, quoted=False):
		statement = map(self._fromNode, statement)
		identifier = self._getIdentifier(context)
		if identifier:
			args = list(statement) + [colon(identifier, self.index)]
			func = rdf_assert4(*args)
		else:
			func = rdf_assert(*statement)
		if self.attached:
			func = rdf_transaction(func, log(message(self.index)))
		call(func, module="rdf_db")
		self.index += 1
	def remove(self, statement, context=None):
		statement = map(self._fromNode, statement)
		identifier = self._getIdentifier(context)
		if identifier:
			args = list(statement) + [colon(identifier, self.index)]
			func = rdf_retractall4(*args)
		else:
			func = rdf_retractall(*statement)
		if self.attached:
			func = rdf_transaction(func, log(message(self.index)))
		call(func, module="rdf_db")
		self.index += 1

	def triples(self, statement, context=None):
		statement = map(self._fromNode, statement)
		identifier = self._getIdentifier(context)
		G, L = Variable(), Variable()

		if self.entailment:
			E = Variable()
			query = entailment2(Atom(self.entailment), E)
			try:
				module = [E.value for x in Query(query, module="serql")][0]
			except IndexError:
				raise PrologError("Unknown Entailment: %s" % entailmod)
			query = rdf(*statement)
		else:
			if identifier:
				args = list(statement) + [colon(identifier,L)]
			else:
				args = list(statement) + [colon(G,L)]
			query = rdf4(*args)
			module = "rdf_db"

		for i in Query(query, module=module):
			row = [self._toNode(X.value)
				if isinstance(X, Variable)
				else self._toNode(X)
				for X in statement]
			yield row, context

	def load(self, filename, context=None):
		file = Atom(filename)
		identifier = self._getIdentifier(context)
		options = []
		if identifier:
			options.append(db(identifier))
		call(rdf_load(file, options), module="rdf_db")
	def unload(self, context=None):
		identifier = self._getIdentifier(context)
		if identifier:
			call(rdf_unload(identifier))

	def query(self, q, entailmod="rdf"):
		entailmod = Atom(entailmod)
		q = Atom(q)
		R = Variable()
		for i in Query(sparql_query(q, R, [entailment(entailmod)]), module="sparql"):
			result = R.value
			if isinstance(result, Atom) and result in (true, false):
				yield result
			elif isinstance(result, Term):
				yield [self._toNode(x.value) for x in result.args]
			else:
				raise ValueError("bad result, %s %s" % (result, type(result)))
			

	def _getIdentifier(self, context):
		if isinstance(context, Graph):
			identifier = context.identifier
		else:
			identifier = context
		if identifier:
			return Atom(identifier)
	def _fromNode(self, node):
		if node is None:
			return Variable()
		elif isinstance(node, URIRef):
			return Atom(str(node))
		elif isinstance(node, Literal):
			if not node.language and not node.datatype:
				return literal(Atom(str(node)))
			if node.language and not node.datatype:
				return literal(lang(Atom(node.language), Atom(str(node))))
			if not node.language and node.datatype:
				return literal(dtype(Atom(str(node.datatype)), Atom(str(node))))
		raise ValueError("Wrong node: %s" % (repr(node),))
	def _toNode(self, term):
		if isinstance(term, Atom):
			return URIRef(str(term))
		elif isinstance(term, Term): ##
			name = term.functor.name
			if name == "literal":
				term = term.args[0].value
				if isinstance(term, Atom):
					return Literal(str(term))
				if isinstance(term, Term):
					name = term.functor.name
					args = term.args
					if name == "lang":
						lang = str(args[0].value)
						value = args[1].value.name
						return Literal(value, lang=lang)
					if name == "type":
						dtype = URIRef(str(args[0].value))
						value = URIRef(str(args[1].value))
						return Literal(value, datatype=dtype)
			elif name == ":":
				## args[1] is normally line number
				args = term.args
				return URIRef(str(args[0].value)), args[1].value
		elif isinstance(term, Variable):
			return self._toNode(term.value)
		raise ValueError("Wrong Term: %s" % (repr(term),))

register("SWIStore", Store, "swirdf", "SWIStore")
