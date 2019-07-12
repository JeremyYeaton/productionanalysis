# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 13:04:23 2019

@author: jdyea
"""
import os, csv, numpy as np
os.chdir('C:\\Users\\jdyea\\OneDrive\\Schoolwork\\Deprez & Aresty\\productionanalysis\\rproj\\data')
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
#%%
f = open('master.csv','r')
g = csv.reader(f,delimiter = ',')
#%%
a = np.arange(50,110)
b = np.arange(50,100)
c = np.arange(50,90)
d = np.arange(50,80)
e = np.arange(50,70)

aa = np.arange(50,110,3)
bb = np.arange(50,100,5/2)
cc = np.arange(50,90,2)
dd = np.arange(50,80,3/2)
bb = [int(i) for i in bb]
dd = [int(i) for i in dd]