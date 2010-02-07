from rdflib.graph import Graph
from rdflib.store import Store
from rdflib.plugin import register
from rdflib.term import URIRef, Literal, BNode
from swipy import *

__all__ = ["SWIStore"]

call(use_module(library(Atom("semweb/rdf_db"))))
rdf = Functor("rdf", 3)
rdf4 = Functor("rdf", 4)
rdf_assert = Functor("rdf_assert", 3)
rdf_assert4 = Functor("rdf_assert", 4)
rdf_load = Functor("rdf_load", 2)
literal = Functor("literal")
lang = Functor("lang", 2)
dtype = Functor("type", 2)
graph = Functor("graph")
db = Functor("db")
colon = Functor(":", 2)

class SWIStore(Store):
	context_aware = True
	def __init__(self):
		self.index = 0
	def add(self, statement, context=None, quoted=False):
		statement = map(self._fromNode, statement)
		identifier = self._getIdentifier(context)
		if identifier:
			args = list(statement) + [colon(identifier, self.index)]
			call(rdf_assert4(*args))
		else:
			call(rdf_assert(*statement))
		self.index += 1
	def triples(self, statement, context=None):
		statement = map(self._fromNode, statement)
		identifier = self._getIdentifier(context)
		G, L = Variable(), Variable()
		if identifier:
			args = list(statement) + [colon(identifier,L)]
		else:
			args = list(statement) + [colon(G,L)]
		query = rdf4(*args)
		for i in Query(query):
			row = [self._toNode(X.value)
				if isinstance(X, Variable)
				else self._toNode(X)
				for X in statement]
			if not identifier:
				context = (str(G.value), L.value)
			yield row, context

	def load(self, filename, context=None):
		file = Atom(filename)
		identifier = self._getIdentifier(context)
		options = []
		if identifier:
			options.append(db(identifier))
		call(rdf_load(file, options))

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
						lang = args[0].value
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
		raise ValueError("Wrong Term: %s" % (repr(term),))

register("SWIStore", Store, "swipy.store", "SWIStore")
