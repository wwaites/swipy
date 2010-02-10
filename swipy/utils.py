from swi import Frame

__all__ = ["framed", "framed_generator"]

def framed(f):
	"""
	Wrap the function in a Prolog Foreign Frame so that
	memory associated with any references that are acquired
	is freed.
	"""
	def _f(*av, **kw):
		frame = Frame()
		result = f(*av, **kw)
		frame.discard()
		return result
	_f.__doc__ = f.__doc__
	_f.func_name = f.func_name
	return _f

def framed_generator(f):
	"""
	Same as the framed decorator but for generators.
	"""
	def _f(*av, **kw):
		frame = Frame()
		for result in f(*av, **kw):
			yield result
		frame.discard()
	_f.__doc__ = f.__doc__
	_f.func_name = f.func_name
	return _f	
