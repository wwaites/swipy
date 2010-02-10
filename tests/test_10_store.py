from rdflib.graph import Graph, ConjunctiveGraph
from rdflib.namespace import Namespace, RDFS
from rdflib.term import Node, URIRef
from swipy.store import SWIStore
from os import path, stat

owl_test = path.join(path.dirname(__file__), "owl.rdf")
skos_test = path.join(path.dirname(__file__), "skos.rdf")
cofog_test = path.join(path.dirname(__file__), "cofog-1999.rdf")
entail_test = path.join(path.dirname(__file__), "entail.n3")

class TestClass:
	def test_00_graph(self):
		store = SWIStore()
		graph = Graph(store)
		graph = Graph("SWIStore")
	def test_01_parse(self):
		graph = Graph("SWIStore", identifier="test")
		graph.parse(cofog_test)
	def test_02_query(self):
		graph = Graph("SWIStore", identifier="test")
		for i, statement in enumerate(graph.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 572
		graph.store.unload(graph)
	def test_03_load(self):
		graph = Graph("SWIStore", identifier="test")
		graph.store.load(cofog_test, graph)
		for i, statement in enumerate(graph.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 572

	def test_04_triples(self):
		def _count():
			i = 0
			for i, (statement, ctx) in enumerate(store.triples((None, None, None))):
				for n in statement:
					assert isinstance(n, Node)
			return i
		store = SWIStore()
		assert _count() == 2854

	def test_05_persist(self):
		graph = Graph("SWIStore", identifier="test")
		assert not graph.store.attached
		graph.open("test.db")
		assert graph.store.attached
		graph.parse(cofog_test)
		stat("test.db")
		graph.close()

	def test_06_retract(self):
		graph = Graph("SWIStore", identifier="test")
		graph.open("test.db")
		ntriples = len(list(graph.triples((None, RDFS.label, None))))
		assert ntriples > 0
		graph.remove((None, RDFS.label, None))
		ntriples = len(list(graph.triples((None, RDFS.label, None))))
		assert ntriples == 0
		graph.store.unload(graph)
		graph.close()

	def test_07_namespaces(self):
		s = SWIStore()
		s.bind("example", "http://example.org/")
		pfx = s.prefix("http://example.org/")
		assert pfx == "example"
		uri = s.namespace("example")
		assert uri == URIRef("http://example.org/")

	def test_08_n3(self):
		st = SWIStore()
		g = Graph(st, identifier="test")
		st.remove((None,None,None))
		g.parse(entail_test, format="n3")
		st.compile()
		st.entailment = "n3"
		assert len(list(g.triples((None, None, None)))) == 2
		st.remove((None,None,None))
