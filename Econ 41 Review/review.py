import matplotlib.pyplot as plt
import numpy as np
import scipy.stats as stats
import math

mu = 0
variance = 1
sigma = math.sqrt(variance)
x = np.linspace(mu - 3*sigma, mu + 3*sigma, 100)
x2 = np.linspace(mu+1 - 3*sigma, mu+1 + 3*sigma, 100)
plt.figure(dpi=300)
plt.axvline(x=0, color="#1f77b4", linestyle = "dotted")
plt.axvline(x=1,color="#ff7f0e", linestyle = "dotted")
plt.plot(x, stats.norm.pdf(x, mu, sigma), label = "X ~ N(0,1)")
plt.plot(x2, stats.norm.pdf(x2, mu+1, sigma), label = "Y = X + 1")
plt.legend(loc="best")
plt.title("Both X and X+1 are equally spread around their means")
plt.xlabel("X")
plt.ylabel("Distribution")
plt.savefig("X-dist")
plt.show()


