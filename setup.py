from numpy.distutils.core import setup, Extension

version = open('VERSION').read().strip('\n')

# -g compiles with debugging information.
# -O0 means compile with no optimization, try -O3 for blazing speed
compile_args = ['-O3']
ext = []
ext.append(Extension('DC3D',
                  sources = ['okada_wrapper/DC3D.f',
                             'okada_wrapper/DC3D.pyf'],
                  extra_compile_args=compile_args))

try:
   import pypandoc
   description = pypandoc.convert('README.md', 'rst')
except (IOError, ImportError):
   description = open('README.md').read()

setup(
   packages = ['okada_wrapper'],
    install_requires = ['numpy'],
   zip_safe = False,
   ext_modules=ext,

   name = "okada_wrapper",
   version = version,
   description = 'Python and MATLAB wrappers for the Okada Green\'s function codes',
   # long_description = description,

   url = 'https://github.com/tbenthompson/okada_wrapper',
   author = 'Ben Thompson',
   author_email = 't.ben.thompson@gmail.com',
   license = 'MIT',
   keywords = ['okada', 'elastic', 'halfspace'],
   classifiers = []
)
