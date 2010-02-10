from rdflib.graph import Graph
from rdflib.store import Store
from rdflib.plugin import register
from rdflib.term import URIRef, Literal, BNode
from swi import *
from swipy.utils import *
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

set_path("serql", "prolog/SeRQL")
set_path("library", "prolog/SeRQL/lib")
set_path("henry", "prolog/henry")
serql = Functor("serql")
henry = Functor("henry")

##
## Read in Prolog Dependencies (mind namespaces, each in own module)
##
deps = (
	(swi, swi_name + ".rc"),
	(library, "semweb/rdf_db"),
	(library, "semweb/rdf_persistency"),
	(library, "semweb/rdf_portray"),
	(serql, "load"),
	(henry, "n3_load"),
	(henry, "n3_to_prolog"),
	(henry, "n3_dcg"),
	(henry, "n3_entailment"),
	(henry, "rdf_e"),
)

## Why do we need to set verbose here? if we don't we get
## errors about list_to_conj and get_list not found. Both
## are defined in henry/utils.pl, even if we load it explicitly
## we have the same problem.
prolog_flags.verbose = 'normal'
for loc, name in deps:
	mod = name.split("/")[-1]
	call(load_files([loc(Atom(name))]), module=mod)
prolog_flags.verbose = 'silent'

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
## Functors for namespace handling
##
rdf_current_ns = Functor("rdf_current_ns", 2)
rdf_register_ns = Functor("rdf_register_ns", 3)
ns_force = Functor("force")
ns_keep = Functor("keep")

##
## Functors from Henry
##
n3_load = Functor("n3_load")
compile_all = Functor("compile_all", 0)

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
	"""
	RDFLib Store backed by SWI-Prolog

	Assigning a known entailment type to the entailment method will
	cause the triples() and query() methods to return entailed triples
	in addition to the ones asserted directly in the store. Entailment
	methods are those known to the SeRQL module.

	The open and close methods activate (and deactivate) persistence
	for the store. If a store is attached using the open method any
	modifications to the store are run inside a transaction, which
	itself enables journalling.
	"""
	context_aware = True
	def __init__(self):
		self.index = 0
		self.entailment = None

	@framed
	def open(self, directory, create=False):
		"""
		Make the store persistent by attaching to the given directory
		"""
		call(rdf_attach_db(Atom(directory), []), module="rdf_persistency")
	@framed
	def close(self, commit_pending_transaction=False):
		"""
		If the store is attached, detatch it
		"""
		if self.attached:
			call(rdf_detach_db(), module="rdf_persistency")
	@framed
	def attached(self):
		X = Variable()
		backing = [X.value for x in Query(rdf_current_db(X), module="rdf_persistency")]
		if backing: result = backing[0]
		else: result = None
		return result
	attached = property(attached)

	@framed
	def load(self, filename, context=None, format="xml"):
		"""
		Load the given file into the store. Ignores the context argument.
		This supports XML files via rdf_db:rdf_load as well as N3 files
		via henry:n3_load according to the format argument. This is much
		faster than using graph.parse()
		"""
		file = Atom(filename)
		if format == "xml":
			identifier = self._getIdentifier(context)
			options = []
			if identifier:
				options.append(db(identifier))
			call(rdf_load(file, options), module="rdf_db")
		elif format == "n3":
			call(n3_load(file), module="n3_load")
			call(compile_all(), module="n3_to_prolog")
	@framed
	def unload(self, context=None):
		"""
		Unload any asserted triples in the named graph.
		"""
		identifier = self._getIdentifier(context)
		if identifier:
			call(rdf_unload(identifier), module="rdf_db")

	@framed
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
	@framed
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

	@framed_generator
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

	@framed_generator
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
			return URIRef(term.name)
		elif isinstance(term, Term): ##
			name = term.functor.name
			if name == "literal":
				term = term.args[0].value
				if isinstance(term, Atom):
					return Literal(term.name)
				if isinstance(term, Term):
					name = term.functor.name
					args = term.args
					if name == "lang":
						lang = args[0].value.name
						value = args[1].value.name
						return Literal(value, lang=lang)
					if name == "type":
						dtype = URIRef(args[0].value.name)
						value = URIRef(args[1].value.name)
						return Literal(value, datatype=dtype)
			elif name == ":":
				## args[1] is normally line number
				args = term.args
				return URIRef(args[0].value.name), args[1].value
		elif isinstance(term, Variable):
			return self._toNode(term.value)
		return Literal(term) ## try our best

	##
	## Support for Namespaces
	##
	@framed_generator
	def namespaces(self):
		NS, URI = Variable(), Variable()
		for x in Query(rdf_current_ns(NS, URI), module="rdf_db"):
			yield str(NS.value), self._toNode(URI.value)
	@framed
	def bind(self, ns, uri, force=False):
		if force:
			options = [ns_force(true)]
		else:
			options = [ns_keep(true)]
		call(rdf_register_ns(Atom(str(ns)), Atom(str(uri)), options), module="rdf_db")
	@framed
	def namespace(self, prefix):
		URI = Variable()
		pfx = Atom(str(prefix))
		result = [self._toNode(URI.value) for x in Query(rdf_current_ns(pfx, URI), module="rdf_db")]
		return result[0] if result else None
	@framed
	def prefix(self, uri):
		NS = Variable()
		_uri = Atom(str(uri))
		result = [NS.value.name for x in Query(rdf_current_ns(NS, _uri), module="rdf_db")]
		return result[0] if result else None

register("SWIStore", Store, "swipy.store", "SWIStore")
