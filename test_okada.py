from okada_wrapper import dc3d0wrapper
from numpy import linspace, zeros, log
from matplotlib.pyplot import contourf, xlabel, ylabel, colorbar, show
source_depth = 3.0
obs_depth = 3.0
poisson_ratio = 0.25
mu = 1.0
dip = 90
lmda = 2 * mu * poisson_ratio / (1 - 2 * poisson_ratio)
alpha = (lmda + mu) / (lmda + 2 * mu)
x = linspace(-1, 1, 100)
y = linspace(-1, 1, 100)
ux = zeros((100, 100))
a = zeros(15)
for i in range(100):
    for j in range(100):
        success, u, grad_u = dc3d0wrapper(alpha,
                                           [x[i], y[j], -obs_depth],
                                           [0.0, 0.0, -source_depth],
                                           dip, [1.0, 0.0, 0.0, 0.0]);
        ux[j, i] = u[0]

contourf(x, y, log(abs(ux)))
xlabel('x')
ylabel('y')
colorbar()
show()
