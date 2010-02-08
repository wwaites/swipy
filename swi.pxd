cdef extern from "SWI-Prolog.h":
	##
	ctypedef	long long int64_t
	ctypedef	unsigned long long uint64_t
	## Embedding
	int		PL_initialise(int argc, char **argv)
	int		PL_is_initialised(int *argc, char ***argv)
	int		PL_toplevel()
	int		PL_cleanup(int status)
	void		PL_cleanup_fork()
	void		PL_halt(int status)

	ctypedef	unsigned int *fid_t
	fid_t		PL_open_foreign_frame()
	void		PL_rewind_foreign_frame(fid_t cid)
	void		PL_close_foreign_frame(fid_t cid)
	void		PL_discard_foreign_frame(fid_t cid)

	ctypedef	unsigned int *atom_t
	atom_t		PL_new_atom(char *name)
	char		*PL_atom_chars(atom_t atom)
	void		PL_register_atom(atom_t atom)
	void		PL_unregister_atom(atom_t atom)

	ctypedef	unsigned int *functor_t
	functor_t	PL_new_functor(atom_t atom, int arity)
	atom_t		PL_functor_name(functor_t functor)
	int		PL_functor_arity(functor_t functor)

	ctypedef	unsigned int *term_t
	term_t		PL_new_term_ref()
	term_t		PL_new_term_refs(int n)
	term_t		PL_copy_term_ref(term_t src)
	void		PL_reset_term_refs(term_t term)

	int		PL_term_type(term_t t)
	bint		PL_is_variable(term_t t)
	bint		PL_is_ground(term_t t)
	bint		PL_is_atom(term_t t)
	bint		PL_is_integer(term_t t)
	bint		PL_is_string(term_t t)
	bint		PL_is_float(term_t t)
	bint		PL_is_rational(term_t t)
	bint		PL_is_compound(term_t t)
	bint		PL_is_callable(term_t t)
	bint		PL_is_functor(term_t t, functor_t f)
	bint		PL_is_list(term_t t)
	bint		PL_is_atomic(term_t t)
	bint		PL_is_number(term_t t)
	bint		PL_is_acyclic(term_t t)

	int		PL_put_variable(term_t t)
	int		PL_put_atom(term_t t, atom_t a)
	int		PL_put_atom_chars(term_t t, char *chars)
	int		PL_put_string_chars(term_t t, char *chars)
	int		PL_put_list(term_t t)
	int		PL_put_term(term_t t1, term_t t2)
	int		PL_put_integer(term_t t, long i)
	int		PL_put_functor(term_t t, functor_t f)

	ctypedef struct funcdesc_t:
		atom_t	name
		int	arity
	ctypedef union term_value_t:
		int64_t		i
		double		f
		char		*s
		atom_t		a
		funcdesc_t	t
	int		PL_get_term_value(term_t t, term_value_t *v)		
	int		PL_get_atom(term_t t, atom_t *a)
	int		PL_get_bool(term_t t, bint *i)
	int		PL_get_integer(term_t t, int *i)
	int		PL_get_long(term_t t, long int *i)
	int		PL_get_float(term_t t, double *f)
	int		PL_get_functor(term_t t, functor_t *f)
	int		PL_get_name_arity(term_t t, atom_t *a, int *i)
	int		PL_get_arg(int n, term_t t, term_t a)
	int		PL_get_chars(term_t t, char **s, unsigned int flags)
	int		PL_get_string(term_t t, char **s, size_t *len)
	int		PL_get_string_chars(term_t t, char **s, size_t *len)
	int		PL_get_list(term_t t, term_t v, term_t t)

	int		PL_unify_atom_chars(term_t t, char *s)
	int		PL_unify_integer(term_t t, int i)
	int		PL_unify_bool(term_t t, bint b)
	int		PL_unify_float(term_t t, float f)

	int		PL_cons_functor_v(term_t h, functor_t f, term_t a0)

	ctypedef	void *module_t
	module_t	PL_new_module(atom_t name)

	ctypedef	void *predicate_t
	predicate_t	PL_pred(functor_t f, module_t m)

	ctypedef	unsigned int *qid_t
	qid_t		PL_open_query(module_t m, int flags, predicate_t pred, term_t t0)
	int		PL_next_solution(qid_t qid)
	void		PL_close_query(qid_t qid)
	void		PL_cut_query(qid_t qid)
	term_t		PL_exception(qid_t qid)

	int		PL_call(term_t t, module_t m)

	int		PL_put_nil(term_t l)
	int		PL_cons_list(term_t h, term_t v, term_t t)

	### constants
	enum:	PL_VARIABLE
	enum:	PL_ATOM
	enum:	PL_INTEGER
	enum:	PL_FLOAT
	enum:	PL_STRING
	enum:	PL_TERM
	enum:	PL_FUNCTOR
	enum:	PL_LIST
	enum:	PL_CHARS
	enum:	PL_POINTER
	enum:	PL_CODE_LIST
	enum:	PL_CHAR_LIST
	enum:	PL_BOOL
	enum:	PL_FUNCTOR_CHARS
	enum:	_PL_PREDICATE_INDICATOR
	enum:	PL_SHORT
	enum:	PL_INT
	enum:	PL_LONG
	enum:	PL_DOUBLE
	enum:	PL_NCHARS
	enum:	PL_UTF8_CHARS
	enum:	PL_UTF8_STRING
	enum:	PL_INT64
	enum:	PL_NUTF8_CHARS
	enum:	PL_NUTF8_CODES
	enum:	PL_NUTF8_STRING
	enum:	PL_NWCHARS
	enum:	PL_NWCODES
	enum:	PL_NWSTRING
	enum:	PL_MBCHARS
	enum:	PL_MBCODES
	enum:	PL_MBSTRING
	enum:	PL_INTPTR
	enum:	PL_CHAR
	enum:	PL_CODE
	enum:	PL_BYTE
	enum:	PL_PARTIAL_LIST
	enum:	PL_CYCLIC_TERM
	enum:	PL_NOT_A_LIST

	enum:	CVT_ATOM
	enum:	CVT_STRING
	enum:	CVT_LIST
	enum:	CVT_INTEGER
	enum:	CVT_FLOAT
	enum:	CVT_VARIABLE
	enum:	CVT_NUMBER
	enum:	CVT_ATOMIC
	enum:	CVT_WRITE
	enum:	CVT_ALL
	enum:	CVT_MASK
	enum:	CVT_EXCEPTION

	enum:	BUF_DISCARDABLE
	enum:	BUF_RING
	enum:	BUF_MALLOC

	enum:	PL_Q_NORMAL
	enum:	PL_Q_NODEBUG
	enum:	PL_Q_CATCH_EXCEPTION
	enum:	PL_Q_PASS_EXCEPTION
