from rdflib.graph import Graph
from swirdf import SWIStore
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
		graph = Graph(store)
		assert _count() == 3560
		open("test.n3", "w+").write(graph.serialize(format="n3"))
		store.entailment = "none"
		assert _count() == 3558
		open("test-none.n3", "w+").write(graph.serialize(format="n3"))
		store.entailment = "rdf"
		assert _count() == 5871
		open("test-rdf.n3", "w+").write(graph.serialize(format="n3"))
		store.entailment = "rdfs"
		assert _count() == 6562
		open("test-rdfs.n3", "w+").write(graph.serialize(format="n3"))
