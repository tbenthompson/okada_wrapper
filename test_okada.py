from DC3D import dc3d0
from numpy import linspace, zeros, log
from matplotlib.pyplot import contourf, xlabel, ylabel, colorbar, show
print dc3d0.__doc__
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
        ux[i, j], a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11] =\
            dc3d0(alpha, x[i], y[j], -obs_depth, source_depth, dip, 1.0, 0.0, 0.0, 0.0);

contourf(x, y, log(abs(ux)))
xlabel('x')
ylabel('y')
colorbar()
show()
