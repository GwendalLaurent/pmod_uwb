from itertools import count
import matplotlib.pyplot as plt
from matplotlib.rcsetup import np
import pandas as pd
import sys

measurements = pd.read_csv(sys.argv[1],names=["Measurements"], index_col=False).iloc[::-1]
print(measurements)
avg = measurements["Measurements"].mean()

plt.plot(measurements, label="measured distances")
# plt.axhline(y=avg, color="r", label="Average")
plt.xlabel("Time")
plt.ylabel("Distance (m)")
plt.legend()
plt.show(block=True)

rounded = measurements.round(2)
c = rounded["Measurements"].value_counts().sort_index()
print(c)
c.plot.bar()
plt.show(block=True)
