from rdflib.graph import Graph
from swipy.store import SWIStore
from swipy import *
from os import path

cofog_test = path.join(path.dirname(__file__), "cofog-1999.rdf")

class TestClass:
	def test_00_graph(self):
		graph = Graph("SWIStore")
		graph.store.load(cofog_test, graph)
		graph.store.query("ASK WHERE { ?s ?p ?o }")
