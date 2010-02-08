from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import sys, os

version = '0.2'

def swipl_config():
	vars = os.popen("swipl -dump-runtime-variables").read()
	lines = [line.split("=", 1) for line in vars.split("\n") if line]
	config = dict([(x, eval(y.rstrip(";"))) for x,y in lines])
	include_dir = os.path.join(config["PLBASE"], "include")
	library_dir = os.path.join(config["PLBASE"], "lib")
	library_dir = os.path.join(library_dir, config["PLARCH"])
	return {
		"include_dirs" : [include_dir],
		"library_dirs" : [library_dir]
	}

swi = Extension(
        name="swi",
        sources=[
                "swi.pyx",
        ],
        extra_compile_args=[],
        define_macros=[],
        libraries=["pl"],
	**swipl_config()
)

setup(name='swipy',
      version=version,
      description="SWI Prolog Python Bindings + RDFLib Store",
      long_description="""\
SWI Prolog Python Bindings
... and RDFLib store""",
      classifiers=[],
      keywords='prolog, rdf',
      author='William Waites',
      author_email='wwaites_at_gmail.com',
      url='http://github.com/wwaites/swipy',
      license='GPL',
      packages=["swirdf"],
      cmdclass={'build_ext': build_ext},
      ext_modules=[swi],
      package_data={
		"swirdf": [
			"henry/*.pl", "henry/AUTHORS", "henry/README", "henry/COPYING",
			"SeRQL/*.pl"],
      }
)
