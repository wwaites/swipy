from rdflib.graph import Graph
from swipy import SWIStore
from os import path, stat

owl_test = path.join(path.dirname(__file__), "owl.rdf")
skos_test = path.join(path.dirname(__file__), "skos.rdf")
cofog_test = path.join(path.dirname(__file__), "cofog-1999.rdf")

class TestClass:
	def test_00_triples(self):
		def _count():
			i = 0
			for i, (statement, ctx) in enumerate(store.triples((None, None, None))):
					pass
			return i
		store = SWIStore()
		store.load(owl_test)
		store.load(skos_test)
		store.load(cofog_test)
		assert _count() == 2854
		store.entailment = "none"
		assert _count() == 2854
		store.entailment = "rdf"
		assert _count() == 6096
		store.entailment = "rdfs"
		assert _count() == 2854