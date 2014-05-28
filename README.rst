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
