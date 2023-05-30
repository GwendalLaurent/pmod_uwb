from itertools import count
import matplotlib.pyplot as plt
from matplotlib.rcsetup import np
import pandas as pd
import sys


measurements = []
with open(sys.argv[1]) as file:
    measurements = [float(elem.strip()) for elem in file.readlines()[::-1]]

print(measurements)

avg = np.average(measurements)
plt.plot(measurements, label="measured distances")
# plt.axhline(y=avg, color="r", label="Average")
plt.xlabel("Time")
plt.ylabel("Distance (m)")
plt.legend()
plt.show(block=True)

# rounded = measurements.round(2)
# c = rounded["Measurements"].value_counts().sort_index()
# print(c)
# c.plot.bar()
# plt.show(block=True)
