import numpy as np
import pandas as pd
import matplotlib; matplotlib.rcParams["savefig.directory"] = "."
from matplotlib import pyplot as plt
plt.rcParams.update({'font.size': 18})
from datetime import datetime, timedelta

distances = [0, 8.4, 18.7, 23, 33.1, 42.4,50,57.5,62,71.5,77,83.9,89.2,94.8,102]
aid_stations = ["Gooseberry", "Split Rock", "Beaver Bay", "Tettegouche", "County rd 6", "Finland", "Sonju", "Crosby", "Sugar Loaf", "Cramer Road", "Temperance", "Sawbill", "Oberg", "Caribou Highlands"]

total_time = timedelta(hours=32)

start = datetime(2022,9,9,7,50)
print(start)
print(start+total_time)

for dist, station in zip(distances, aid_stations):
    print(station, start+total_time*dist/distances[-1])
