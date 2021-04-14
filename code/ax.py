import numpy as np
import pandas as pd
import matplotlib; matplotlib.rcParams["savefig.directory"] = "."
from matplotlib import pyplot as plt
#plt.style.use('classic')

def make_cmap(fn, c0, c1, label):
    fig = plt.figure()
    img = plt.imshow(np.array((np.linspace(c0,c1,6),)).T, cmap='jet')
    img.set_visible(False)
    plt.gca().axis('off')
    clb = plt.colorbar(ticks=np.linspace(c0,c1,6))
    clb.ax.set_ylabel(label, fontsize=18)
    clb.ax.tick_params(labelsize=18)
    clb.ax.set_yticklabels(["{:2.1e}".format(i) for i in np.linspace(c0,c1,6)])
    fig.tight_layout()
    plt.savefig(fn+".png")
    plt.close(fig)

make_cmap("cb4", 25, 100, "Temperature [C]")
make_cmap("cb6", 1e-10, 1e-3, "Aperture [m]")
make_cmap("cb6", 1e-10, 1e-3, "Aperture [m]")
make_cmap("cb7", 25, 200, "Temperature [C]")
