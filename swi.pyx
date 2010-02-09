cimport swi

class PrologError(Exception):
	"""Exception in Prolog"""

cdef class Frame:
	cdef fid_t _cid
	cdef bool _open
	def __cinit__(self):
		self._cid = swi.PL_open_foreign_frame()
		self._open = True
	def __dealloc__(self):
		if self._open:
			self.discard()
	def discard(self):
		if self._open:
			self._open = False
			swi.PL_discard_foreign_frame(self._cid)
		else:
			raise PrologError("Frame Already Closed")
	def rewind(self):
		if self._open:
			self._open = False
			swi.PL_rewind_foreign_frame(self._cid)
		else:
			raise PrologError("Frame Already Closed")
	def close(self):
		if self._open:
			self._open = False
			swi.PL_close_foreign_frame(self._cid)
		else:
			raise PrologError("Frame Already Closed")

cdef class Atom:
	cdef atom_t _atom
	def __cinit__(self, char *name=NULL):
		if name:
			self._atom = swi.PL_new_atom(name)
	def __dealloc__(self):
		if self._atom:
			swi.PL_unregister_atom(self._atom)
	cdef ref(self, atom_t atom):
		swi.PL_register_atom(atom)
		self._atom = atom
		return self
	def name(self):
		cdef char *name
		if self._atom:
			name = <char *>swi.PL_atom_chars(self._atom)
			try:
				return name.decode("utf-8")
			except UnicodeDecodeError:
				return name.decode("latin1")
		else:
			return "(null)"
	name = property(name)
	def __str__(self):
		return str(self.name)
	def __repr__(self):
		return "Atom('%s')" % self
	def __richcmp__(self, other, op):
		if op == 2:
			return isinstance(other, Atom) and self.name == other.name
		raise ValueError("Cannot compare, %s <%s> %s" % (self, op, other))

cdef class Functor:
	cdef functor_t _functor
	cdef Atom _atom
	cdef public int arity
	def __cinit__(self, atom=None, int arity=1):
		if atom:
			if not isinstance(atom, Atom):
				atom = Atom(str(atom))
			self._functor = swi.PL_new_functor((<Atom>atom)._atom, arity)
			self.ref(self._functor)
	cdef ref(self, functor_t f):
		self._functor = f
		self._atom = Atom().ref(swi.PL_functor_name(f))
		self.arity = swi.PL_functor_arity(self._functor)
		return self
	def __str__(self):
		return "%s/%d" % (self._atom, self.arity)
	def __repr__(self):
		return "Functor('%s', %d)" % (self._atom, self.arity)
	def __call__(self, *args):
		cdef int nargs = len(args)
		assert self.arity == nargs
		cdef term_t a = swi.PL_new_term_refs(nargs)
		cdef int i
		for i,arg in enumerate(args):
			Term().ref(a+i).value = arg
		cdef term_t t = swi.PL_new_term_ref()
		swi.PL_cons_functor_v(t, self._functor, a)
		return Term().ref(t)
	def name(self):
		return str(self._atom)
	name = property(name)
		

cdef class Term:
	cdef term_t _term
	def __cinit__(self, value=None):
		if value:
			self._term = PL_new_term_ref()
			self.value = value
	cdef ref(self, term_t term):
		self._term = term
		return self
	def set_value(self, value):
		if isinstance(value, Term):
			swi.PL_put_term(self._term, (<Term>value)._term)
		elif isinstance(value, Atom):
			swi.PL_put_atom(self._term, (<Atom>value)._atom)
		elif isinstance(value, Functor):
			swi.PL_put_functor(self._term, (<Functor>value)._functor)
		elif isinstance(value, basestring):
			swi.PL_put_string_chars(self._term, value)
		elif isinstance(value, int):
			swi.PL_put_integer(self._term, value)
		elif isinstance(value, Variable):
			(<Variable>value).ref(self._term)
			#swi.PL_put_variable(self._term)
		elif isinstance(value, list) or isinstance(value, tuple):
			swi.PL_put_nil(self._term)
			for item in reversed(value):
				swi.PL_cons_list(self._term, (<Term>Term(item))._term, self._term)
		else:
			raise ValueError("Term unimplemented %s: %s" % (type(value), value))
	def get_value(self):
		cdef int t
		cdef term_value_t val
		cdef atom_t a
		cdef term_t arg
		cdef term_t head
		cdef term_t tail

		t = swi.PL_term_type(self._term)
		if t == PL_VARIABLE:
			return Variable().ref(self._term)

		PL_get_term_value(self._term, &val)
		if t in (PL_INTEGER, PL_LONG):
			return val.i
		elif t in (PL_FLOAT, PL_DOUBLE):
			return val.f
		elif t == PL_STRING:
			return '"' + val.s + '"'
		elif t == PL_ATOM:
			return Atom().ref(val.a)
		elif PL_is_list(self._term):
			tail = PL_copy_term_ref(self._term)
			head = PL_new_term_ref()
			result = []
			while PL_get_list(tail, head, tail):
				result.append(Term().ref(head))
				head = PL_new_term_ref()
			PL_reset_term_refs(tail)
			return result
		elif t == PL_TERM:
			return self
		raise ValueError("Term unimplemented: type %d" % t)
	value = property(get_value, set_value)

	def functor(self):
		cdef functor_t f
		assert swi.PL_get_functor(self._term, &f)
		return Functor().ref(f)
	functor = property(functor)
	def args(self):
		functor = self.functor
		cdef term_t arg
		ret = []
		for n in range(1, functor.arity + 1):
			arg = PL_new_term_ref()
			swi.PL_get_arg(n, self._term, arg)
			ret.append(Term().ref(arg))
		return tuple(ret)
	args = property(args)

	def __str__(self):
		cdef int t
		t = swi.PL_term_type(self._term)
		if t == PL_TERM and not PL_is_list(self._term):
			ret = "%s(" % self.functor.name
			ret += ", ".join(map(str, self.args))
			ret += ")"
			return ret
		else:
			return str(self.value)
	def __repr__(self):
		return "Term(%d)" % <unsigned long>self._term

cdef class Variable:
	cdef term_t _term
	def __cinit__(self):
		self._term = PL_new_term_ref()
	cdef ref(self, term_t term):
		self._term = term
		return self
	def set_value(self, value):
		cdef term_t t
		if isinstance(value, basestring):
			t = PL_new_term_ref()
			swi.PL_unify_atom_chars(t, value)
		elif isinstance(value, int):
			t = PL_new_term_ref()
			swi.PL_unify_integer(t, value)
		elif isinstance(value, bool):
			t = PL_new_term_ref()
			swi.PL_unify_bool(t, value)
		elif isinstance(value, float):
			t = PL_new_term_ref()
			swi.PL_unify_float(t, value)
		else:
			raise ValueError("Variable unimplemented %s: %s" % (type(value), value))
		self.ref(t)
	def get_value(self):
		return Term().ref(self._term).value
	value = property(get_value, set_value)

	def __str__(self):
		return "V(%d)" % (<int>self._term)
	def __repr__(self):
		return str(self)

cdef class Query:
	cdef qid_t _qid
	def __cinit__(self, *terms, module=None):
		#, int flags=PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION, module_t module=NULL):
		cdef module_t mod
		cdef atom_t modname
		if module:
			if isinstance(module, Atom):
				modname = (<Atom>module)._atom
			else:
				modname = (<Atom>Atom(module))._atom
			mod = PL_new_module(modname)
		else:
			mod = NULL

		t = terms[0]
		for tx in terms[1:]:
			t = comma(t, tx)
		f = t.functor

		cdef int flags = PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION
		cdef predicate_t p = swi.PL_pred((<Functor>f)._functor, mod)
		cdef int arity = (<Functor>f).arity
		cdef term_t term = (<Term>t)._term
		cdef term_t a0 = swi.PL_new_term_refs(arity)
		for i, a in enumerate(range(1, arity+1)):
			swi.PL_get_arg(a, term, a0+<int>i)
		self._qid = swi.PL_open_query(mod, flags, p, a0)
	def __dealloc__(self):
		if self._qid:
			swi.PL_close_query(self._qid)
	def __iter__(self):
		return self
	def __next__(self):
		if PL_next_solution(self._qid):
			return
		if PL_exception(self._qid):
			raise PrologError("Bad Query")
		raise StopIteration
	def cut(self):
		PL_cut_query(self._qid)

def call(*terms, module=None):
	cdef module_t mod
	cdef atom_t modname
	if module:
		if isinstance(module, Atom):
			modname = (<Atom>module)._atom
		else:
			modname = (<Atom>Atom(module))._atom
		mod = PL_new_module(modname)
	else:
		mod = NULL
	t = terms[0]
	assert isinstance(t, Term)
	for tx in terms[1:]:
		t = comma(t, tx)
	return PL_call((<Term>t)._term, mod)

### housekeeping
cdef _initialise():
	cdef char **argv = ["./", "-q", "-nosignals"]
	swi.PL_initialise(3, argv)
def _finalise():
	swi.PL_halt(0)
_initialise()
import atexit
atexit.register(_finalise)
del _finalise
del atexit

### common functors
comma = Functor(",", 2)
colon = Functor(":", 2)
clause = Functor(":-", 2)

### global atoms
true = Atom("true")
false = Atom("false")
fail = Atom("fail")

asserta = Functor("asserta")
assertz = Functor("assertz")
dynamic = Functor("dynamic")
multifile = Functor("multifile")

consult = Functor("consult")
use_module = Functor("use_module")
load_files = Functor("load_files", 1)

swi = Functor("swi")
library = Functor("library")
foreign = Functor("foreign")
