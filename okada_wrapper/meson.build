py_module = import('python')
py = py_module.find_installation(pure: false)
py_deps = py.dependency()

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy, incdir_f2py)

dc3d_source = custom_target('DC3Dmodule.c',
  input : ['DC3D.f', 'DC3D.pyf'],  # .f so no F90 wrappers
  output : ['DC3Dmodule.c', 'DC3D-f2pywrappers.f'],
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '-m', 'DC3D', '--lower', '--build-dir', 'okada_wrapper']
)

py.install_sources(
  ['__init__.py', 'okada_wrapper.py'],
  subdir: 'okada_wrapper',
  pure: false
)

py.extension_module('DC3D',
  ['DC3D.f', dc3d_source],
  incdir_f2py / 'fortranobject.c',
  include_directories: inc_np,
  subdir: 'okada_wrapper',
  dependencies: py_deps,
  install: true
)

