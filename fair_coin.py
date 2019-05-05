#!/bin/env python
"""
Generate a fair coin from unfair coin
Calculate (0,1) from (0,1) and (1,0)
"""

from scipy.stats import bernoulli

def gen_sample(n,p):
	data_bern1 = bernoulli.rvs(size=n,p=p)
	data_bern2 = bernoulli.rvs(size=n,p=p)
	total = len([(i,j) for (i,j) in zip(data_bern1,data_bern2) if i+j==1])
	event = len([(i,j) for (i,j) in zip(data_bern1,data_bern2) if i==0 and j==1])
	return event/total

print (gen_sample(1000,0.2))
