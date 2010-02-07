from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import sys, os

version = '0.1'

swipy = Extension(
        name="swipy",
        sources=[
                "swipy.pyx",
        ],
        extra_compile_args=[],
        define_macros=[],
        include_dirs=["/opt/local/lib/swipl-5.8.0/include"],
        library_dirs=["/opt/local/lib/swipl-5.8.0/lib/i386-darwin9.8.0"],
        libraries=["pl"],
)

setup(name='swipy',
      version=version,
      description="SWI Prolog Python Bindings",
      long_description="""\
SWI Prolog Python Bindings""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='prolog',
      author='William Waites',
      author_email='wwaites_at_gmail.com',
      url='',
      license='GPL',
      packages=[],
      cmdclass={'build_ext': build_ext},
      ext_modules=[swipy],
)
