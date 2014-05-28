Okada wrapper (MATLAB, Python)
======

These files are MATLAB and Python wrappers for the Okada DC3D0 point source 
fortran subroutines. The Matlab wrappers are written using the MEX functions.
The original subroutine was written by Y. Okada as part of the paper:

Okada, Y., 1992, Internal deformation due to shear and tensile faults in a half-space, 
 Bull. Seism. Soc. Am., 82, 1018-1040. 

In the future, I will add a wrapper for the DC3D rectangular dislocation 
subroutine as well.

MATLAB
----

To get set up with the MATLAB function, open matlab and run::

    mex 'DC3Dwrapper.F'

Then, DC3Dwrapper can be treated like any other MATLAB function::

    [success, u, grad_u] = DC3Dwrapper(0.6, [1.0, 1.0, -1.0],...
                                            [0.0, 0.0, -3.0],...
                                            1.0, [1.0, 0.0, 0.0, 0.0]);

Python
----

For the Python function, download the code::

    git clone https://github.com/tbenthompson/okada_wrapper.git

Then, run the install script::

    python setup.py install

The syntax is almost identical to the MATLAB version::

    success, u, grad_u = dc3d0wrapper(0.6, [1.0, 1.0, -1.0],
                                      [0.0, 0.0, -3.0],
                                      1.0, [1.0, 0.0, 0.0, 0.0]);

Tests
----

To run the tests, from the root directory in MATLAB type::

    test_okada.m

or, from python::
    
    python test_okada.py
