# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 13:04:23 2019

@author: jdyea
"""
#%%
ser = []
for i in range(5,115):
	if (i-5)%10 == 0:
		line = [str(i-5),str(int((i-5)/10)+1)]
	else:
		line = [str(i-5),str(round((i)/10))]
	ser.append(line)
f = open('serAll.csv','w')
f.write('series,syll_num\n')
for line in ser:
	f.write(','.join(line))
	f.write('\n')
f.close()
ser