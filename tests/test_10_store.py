from rdflib.graph import Graph
from rdflib.namespace import Namespace, RDFS
from rdflib.term import Node
from swipy import SWIStore
from os import path, stat

cofog_test = path.join(path.dirname(__file__), "cofog-1999.rdf")

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
	def test_03_load(self):
		graph = Graph("SWIStore")
		graph.store.load(cofog_test, graph)
		for i, statement in enumerate(graph.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 572
		graph.store.unload(graph)
		i = 0
		for i, statement in enumerate(graph.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 0

	def test_04_triples(self):
		store = SWIStore()
		for i, (statement, ctx) in enumerate(store.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 572
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
		assert len(list(graph.triples((None, RDFS.label, None)))) == 0
		graph.close()

