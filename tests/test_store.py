from rdflib.graph import Graph
from rdflib.namespace import Namespace, RDFS
from rdflib.term import Node
from swipy.store import SWIStore
from os import path

class TestClass:
	def test_00_graph(self):
		store = SWIStore()
		graph = Graph(store)
		graph = Graph("SWIStore")
	def test_01_parse(self):
		graph = Graph("SWIStore", identifier="test")
		graph.parse(path.join(path.dirname(__file__), "cofog-1999.rdf"))
	def test_02_query(self):
		graph = Graph("SWIStore", identifier="test")
		for i, statement in enumerate(graph.triples((None, RDFS.label, None))):
			for n in statement:
				assert isinstance(n, Node)
		assert i == 572
