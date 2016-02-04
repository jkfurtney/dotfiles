import numpy as np
import pylab as plt
from scipy.constants import foot, inch

# 18mm by 13.5mm sensor 22.5 mm diagional.
# 20 to 29 mm is "normal" focal length
# Circle of Confusion of micro-four thirds sensor is 0.015mm


CoC = 0.015e-3

f_numbers = [1.8,2.0,2.2,2.5,2.8]
s = np.linspace(0.5,6,50)

focal_length = 45e-3
f = 1.8
for f in f_numbers:
    H = focal_length**2/f/CoC
        #if ss<H:
    Dn = H*s/(H+s)
    Df = H*s/(H-s)
    print Dn, Df
    plt.plot(s/foot, (Df-Dn)/inch, label="f/{}".format(f))
#plt.plot(s, Df, "--")
plt.xlabel("Distance from Camera [ft]")
plt.ylabel("Depth of field [in]")
plt.legend(loc=2)
plt.show()

# f=f_numbers
# focal_length = 50.0e-3
# H = focal_length**2/f/CoC
# mask = s<H
# Dn = H*s/(H+s)
# Df = H*s/(H-s)
# plt.plot(f[mask], Dn[mask]/foot)
# plt.plot(f[mask], Df[mask]/foot)

# f=1.8
# focal_length = 45.0e-3
# H = focal_length**2/f/CoC
# mask = s<H
# Dn = H*s/(H+s)
# Df = H*s/(H-s)
# plt.plot(f[mask], Dn[mask]/foot)
# plt.plot(f[mask], Df[mask]/foot)

# plt.xlabel("f-stop")
# plt.ylabel("Distance from camera [ft]")
# plt.grid()
# plt.show()
