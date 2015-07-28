import numpy as np
import pylab as plt
from scipy.constants import foot

# 18mm by 13.5mm sensor 22.5 mm diagional.
# 20 to 29 mm is "normal" focal length
# Circle of Confusion of micro-four thirds sensor is 0.015mm


CoC = 0.015e-3

focal_length = np.linspace(12e-3,50e-3,100)
f_numbers = np.linspace(3,22,30)
s = 3.0

# for f in f_numbers:
#     H = focal_length**2/f/CoC
#     mask = s<H
#     Dn = H*s/(H+s)
#     Df = H*s/(H-s)
#     plt.plot(focal_length[mask], Dn[mask])
#     plt.plot(focal_length[mask], Df[mask], "--")

# plt.show()

f=f_numbers
focal_length = 50.0e-3
H = focal_length**2/f/CoC
mask = s<H
Dn = H*s/(H+s)
Df = H*s/(H-s)
plt.plot(f[mask], Dn[mask]/foot)
plt.plot(f[mask], Df[mask]/foot)

focal_length = 25.0e-3
H = focal_length**2/f/CoC
mask = s<H
Dn = H*s/(H+s)
Df = H*s/(H-s)
plt.plot(f[mask], Dn[mask]/foot)
plt.plot(f[mask], Df[mask]/foot)

plt.xlabel("f-stop")
plt.ylabel("Distance from camera [ft]")
plt.grid()
plt.show()
