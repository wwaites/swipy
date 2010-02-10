from swi import Frame

__all__ = ["framed", "framed_generator"]

def framed(f):
	def _f(*av, **kw):
		frame = Frame()
		result = f(*av, **kw)
		frame.discard()
		return result
	_f.__doc__ = f.__doc__
	_f.func_name = f.func_name
	return _f

def framed_generator(f):
	def _f(*av, **kw):
		frame = Frame()
		for result in f(*av, **kw):
			yield result
		frame.discard()
	_f.__doc__ = f.__doc__
	_f.func_name = f.func_name
	return _f	
