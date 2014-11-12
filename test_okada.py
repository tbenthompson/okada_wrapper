from okada_wrapper import dc3d0wrapper, dc3dwrapper
from numpy import linspace, zeros, log
from matplotlib.pyplot import contourf, contour,\
    xlabel, ylabel, title, colorbar, show, savefig
import matplotlib
import time
matplotlib.rcParams['font.family'] = 'serif'
matplotlib.rcParams['font.serif'] = ['Computer Modern Roman']
matplotlib.rcParams['text.usetex'] = True
matplotlib.rcParams['font.size'] = 14
matplotlib.rcParams['xtick.direction'] = 'out'
matplotlib.rcParams['ytick.direction'] = 'out'
matplotlib.rcParams['lines.linewidth'] = 1

def get_params():
    source_depth = 3.0
    obs_depth = 3.0
    poisson_ratio = 0.25
    mu = 1.0
    dip = 90
    lmda = 2 * mu * poisson_ratio / (1 - 2 * poisson_ratio)
    alpha = (lmda + mu) / (lmda + 2 * mu)
    return source_depth, obs_depth, poisson_ratio, mu, dip, alpha

def test_dc3d0():
    source_depth, obs_depth, poisson_ratio, mu, dip, alpha = get_params()
    n = (100, 100)
    x = linspace(-1, 1, n[0])
    y = linspace(-1, 1, n[1])
    ux = zeros((n[0], n[1]))
    for i in range(100):
        for j in range(100):
            success, u, grad_u = dc3d0wrapper(alpha,
                                               [x[i], y[j], -obs_depth],
                                               source_depth, dip,
                                               [1.0, 0.0, 0.0, 0.0]);
            assert(success == 0)
            ux[i, j] = u[0]

    cntrf = contourf(x, y, log(abs(ux.T)))
    contour(x, y, log(abs(ux.T)), colors = 'k', linestyles = 'solid')
    xlabel('x')
    ylabel('y')
    cbar = colorbar(cntrf)
    cbar.set_label('$\log(u_{\\textrm{x}})$')
    show()

def test_dc3d():
    source_depth, obs_depth, poisson_ratio, mu, dip, alpha = get_params()
    n = (100, 100)
    x = linspace(-1, 1, n[0])
    y = linspace(-1, 1, n[1])
    ux = zeros((n[0], n[1]))
    for i in range(n[0]):
        for j in range(n[1]):
            success, u, grad_u = dc3dwrapper(alpha,
                                               [x[i], y[j], -obs_depth],
                                               source_depth, dip,
                                               [-0.6, 0.6], [-0.6, 0.6],
                                               [1.0, 0.0, 0.0])
            assert(success == 0)
            ux[i, j] = u[0]

    levels = linspace(-0.5, 0.5, 21)
    cntrf = contourf(x, y, ux.T, levels = levels)
    contour(x, y, ux.T, colors = 'k', levels = levels, linestyles = 'solid')
    xlabel('x')
    ylabel('y')
    cbar = colorbar(cntrf)
    tick_locator = matplotlib.ticker.MaxNLocator(nbins=5)
    cbar.locator = tick_locator
    cbar.update_ticks()
    cbar.set_label('$u_{\\textrm{x}}$')
    savefig("strike_slip.png")
    show()

def test_success():
    # should fail because z is positive
    success, u, grad_u = dc3d0wrapper(1.0, [0.0, 0.0, 1.0],
                                       1.0, 90,
                                       [1.0, 0.0, 0.0, 0.0]);
    assert(success == 2)

    success, u, grad_u = dc3dwrapper(1.0, [0.0, 0.0, 1.0],
                                   0.0, 90, [-0.7, 0.7], [-0.7, 0.7],
                                   [1.0, 0.0, 0.0])
    assert(success == 2)

def benchmark():
    n = 1000000
    start = time.time()
    for i in range(n):
        success, u, grad_u = dc3dwrapper(1.0, [0.0, 0.0, 1.0],
                                         0.0, 90, [-0.7, 0.7], [-0.7, 0.7],
                                         [1.0, 0.0, 0.0])
    end = time.time()
    T = end - start
    print(str(n) + " DC3D evaluations took: " + str(T) + " seconds")
    print("Giving a time of " + str(T / n) + " seconds per evaluation.")

if __name__ == '__main__':
    # test_dc3d0()
    test_dc3d()
    # benchmark()
