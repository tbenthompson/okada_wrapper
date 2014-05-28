from DC3D import dc3d0
from numpy import empty

def dc3d0wrapper(alpha, xo, xs, dip, potency):
    u = empty(3)
    grad_u = empty((3, 3))
    success, u[0], u[1], u[2],\
        grad_u[0, 0], grad_u[0, 1], grad_u[0, 2],\
        grad_u[1, 0], grad_u[1, 1], grad_u[1, 2],\
        grad_u[2, 0], grad_u[2, 1], grad_u[2, 2] = \
        dc3d0(alpha, xo[0] - xs[0], xo[1] - xs[1],
              xo[2], -xs[2], dip, potency[0],
              potency[1], potency[2], potency[3])
    return success, u, grad_u
