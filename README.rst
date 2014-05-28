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

Download the code::

    git clone https://github.com/tbenthompson/okada_wrapper.git

Open matlab and run::

    mex 'DC3Dwrapper.F'

Then, DC3Dwrapper can be treated like any other MATLAB function::

    [success, u, grad_u] = DC3Dwrapper(0.6, [1.0, 1.0, -1.0],...
                                            [0.0, 0.0, -3.0],...
                                            1.0, [1.0, 0.0, 0.0, 0.0]);

The inputs and outputs are slightly different from the okada implementation.
Five arguments are required:

* alpha = (lambda + mu) / (lambda + 2 * mu)
* xo = 3-vector representing the observation point
* xs = 3-vector representing the source point
* dip = the dip-angle of the slip plane in degrees
* potency = 4-vector

  * index 1 = strike-slip = Moment of double-couple / mu
  * index 2 = dip-slip = Moment of double-couple / mu
  * index 3 = inflation = Intensity of isotropic part / lambda
  * index 4 = tensile = Intensity of linear dipole / mu

Three outputs are provided:

* success - a return code from DC3D0=0 if normal, 1 if singular, 2 if a positive z value for the observation point was given
* u - 3-vector representing the displacement at the observation point. for example, u(2) = u_y
* grad_u = the 3x3 tensor representing the partial derivatives of the displacement, for example, grad_u(1, 2) = d^2u/dxdy


Python
----

Download the code::

    git clone https://github.com/tbenthompson/okada_wrapper.git

Then, run the install script::

    python setup.py install

The syntax is almost identical to the MATLAB version::

    success, u, grad_u = dc3d0wrapper(0.6, [1.0, 1.0, -1.0],
                                      [0.0, 0.0, -3.0],
                                      1.0, [1.0, 0.0, 0.0, 0.0])

The arguments and outputs are identical to the MATLAB version.

Tests
----

To run the tests, from the root directory in MATLAB type::

    test_okada.m

or, from python::
    
    python test_okada.py
