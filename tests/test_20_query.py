from rdflib.graph import Graph
from swirdf import SWIStore
from os import path

cofog_test = path.join(path.dirname(__file__), "cofog-1999.rdf")

class TestClass:
	def test_00_ask(self):
		graph = Graph("SWIStore", identifier="qtest")
		graph.store.load(cofog_test, graph)
		assert graph.store.query("ASK WHERE { ?s ?p ?o }")
	def test_01_select(self):
		graph = Graph("SWIStore", identifier="qtest")
		for i,row in enumerate(graph.store.query("SELECT * FROM <qtest> WHERE { ?s ?p ?o }")):
			print row 
			break
		print i
		assert i == 6096

