from numpy.distutils.core import setup, Extension

# -g compiles with debugging information.
# -O0 means compile with no optimization, try -O3 for blazing speed
compile_args = ['-O3']
ext = []
ext.append(Extension('DC3D',
                  sources = ['okada_wrapper/DC3D.f',
                             'okada_wrapper/DC3D.pyf'],
                  extra_compile_args=compile_args))

setup(
   name = "okada_wrapper",
   packages = ['okada_wrapper'],
   version = '0.1.0',
   description = 'Python and MATLAB wrappers for the Okada Green\'s function codes',
   author = 'Ben Thompson',
   author_email = 't.ben.thompson@gmail.com',
   url = 'https://github.com/tbenthompson/okada_wrapper',
   keywords = ['okada', 'elastic', 'halfspace'],
   classifiers = [],
   ext_modules=ext
)
