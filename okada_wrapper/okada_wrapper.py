from DC3D import dc3d0, dc3d
from numpy import empty

def dc3d0wrapper(alpha, xo, depth, dip, potency):
    u = empty(3)
    grad_u = empty((3, 3))
    u[0], u[1], u[2],\
        grad_u[0, 0], grad_u[0, 1], grad_u[0, 2],\
        grad_u[1, 0], grad_u[1, 1], grad_u[1, 2],\
        grad_u[2, 0], grad_u[2, 1], grad_u[2, 2], success = \
        dc3d0(alpha, xo[0], xo[1], xo[2], depth, dip, potency[0],
              potency[1], potency[2], potency[3])
    return success, u, grad_u

def dc3dwrapper(alpha, xo, depth, dip, strike_width, dip_width, dislocation):
    u = empty(3)
    grad_u = empty((3, 3))
    u[0], u[1], u[2],\
        grad_u[0, 0], grad_u[0, 1], grad_u[0, 2],\
        grad_u[1, 0], grad_u[1, 1], grad_u[1, 2],\
        grad_u[2, 0], grad_u[2, 1], grad_u[2, 2], success = \
        dc3d(alpha, xo[0], xo[1], xo[2], depth, dip, strike_width[0],
             strike_width[1], dip_width[0], dip_width[1],
             dislocation[0], dislocation[1], dislocation[2])
    return success, u, grad_u
