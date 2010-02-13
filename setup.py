from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import sys, os, re

version = '0.5'

def swipl_config():
	def get_config(prog):
		vars = os.popen("%s -dump-runtime-variables 2>&1" % prog).read()
		lines = [line.split("=", 1) for line in vars.split("\n") if line]
		return dict([(x, eval(y.rstrip(";"))) for x,y in lines])
	try:
		config = get_config("swipl")
		swipl = "swipl"
	except:
		try:
			config = get_config("pl")
			swipl = "pl"
		except:
			raise RuntimeError("Could not find SWI-Prolog - is it on the PATH?")
	include_dir = os.path.join(config["PLBASE"], "include")
	library_dir = os.path.join(config["PLBASE"], "lib")
	library_dir = os.path.join(library_dir, config["PLARCH"])
	libs = config["PLLIBS"].replace("-l", "").split()
	m = re.match(".*-Wl,-(R|rpath).(?P<dir>[^ ]+).*", config["PLLDFLAGS"]) or []
	runtime_library_dirs = m.groupdict().values() if m else []

	global version
	rev = os.popen("git describe --tags 2> /dev/null").read().strip()
	if rev:
		version = rev
	else:
		rev = os.popen("git describe --always --tags 2> /dev/null").read().strip()
		if rev:
			version = rev
	if rev:
		cfg = open("swi_config.pyx", "w+")
		cfg.write("""\
swi_vers = %s
swi_name = %s
swi_conf = %s
""" % (repr(version), repr(swipl), repr(config)))
	return {
		"include_dirs" : [include_dir],
		"library_dirs" : [library_dir],
		"runtime_library_dirs" : runtime_library_dirs,
		"libraries" : libs,
	}

swi = Extension(
        name="swi",
        sources=[
                "swi.pyx"
        ],
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
			"SeRQL/*.pl", "SeRQL/*/*.pl", "SeRQL/*/*/*.pl"],
      }
)
