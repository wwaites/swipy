from swipy import *

class TestClass(object):
	def test_00_atom(self):
		frame = Frame()

		a = Atom("a")
		assert str(a) == "a"

		frame.discard()

	def test_01_functor(self):
		frame = Frame()

		f = Functor("f")
		assert str(f) == "f/1"
		t = f(1)
		assert str(t) == "f(1)"

		g = Functor("g", 2)
		t = g(t,"hello")
		assert str(t) == 'g(f(1), "hello")'

		frame.discard()

	def test_02_call(self):
		frame = Frame()

		foo = Functor("foo")
		call(assertz(foo(1)))

		frame.discard()

	def test_03_query(self):
		return
		frame = Frame()

		foo = Functor("foo")
		call(assertz(foo(2)))

		X = Variable()
		result = map(lambda x: X.value, Query(foo(X)))
		result.sort()
		assert result == [1,2]

		frame.discard()

	def test_04_use(self):
		frame = Frame()

		libname = Atom("semweb/rdf_db")
		call(use_module(library(libname)))
		rdf = Functor("rdf", 3)

		S,P,O = Variable(), Variable(), Variable()
		result = map(lambda x: None, Query(rdf(S,P,O)))
		assert result == []

		frame.discard()
