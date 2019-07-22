
#%%
get_ipython().run_line_magic('config', 'IPCompleter.greedy=True')

from pulp import *

import numpy as np
import matplotlib.pyplot as plt
%matplotlib inline

'''
trying out PuLP library
'''

lp_brewer = LpProblem("The Brewer Problem",LpMaximize)

ale = LpVariable('Ale', 0)
beer = LpVariable('Beer', 0)

lp_brewer += 13 * ale + 23 * beer, 'total profit to the brewer'

lp_brewer += (ale >=0 and beer >=0), 'non-negative constraint'
lp_brewer += 5 * ale + 15 * beer <=480, 'corn constraint'
lp_brewer += 4 * ale + 4 * beer <=160, 'hops constraint'
lp_brewer += 35 * ale + 20 * beer <= 1190, 'malt constraint'

lp_brewer.writeLP('BrewerProblem.lp')

lp_brewer.solve()

print('LP Status: ', LpStatus[lp_brewer.status])

for var in lp_brewer.variables():
    print(var.name, ' = ', var.varValue)

print(value(lp_brewer.objective))



# Construct lines
# x > 0
x = np.linspace(0, 20, 2000)
# y >= 2
y1 = (x*0) + 2
# 2y <= 25 - x
y2 = (25-x)/2.0
# 4y >= 2x - 8 
y3 = (2*x-8)/4.0
# y <= 2x - 5 
y4 = 2 * x -5

# Make plot
plt.plot(x, y1, label=r'$y\geq2$')
plt.plot(x, y2, label=r'$2y\leq25-x$')
plt.plot(x, y3, label=r'$4y\geq 2x - 8$')
plt.plot(x, y4, label=r'$y\leq 2x-5$')
plt.xlim((0, 16))
plt.ylim((0, 11))
plt.xlabel(r'$x$')
plt.ylabel(r'$y$')

# Fill feasible region
y5 = np.minimum(y2, y4)
y6 = np.maximum(y1, y3)
plt.fill_between(x, y5, y6, where=y5>y6, color='grey', alpha=0.5)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)



#%%



