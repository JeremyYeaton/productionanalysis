from matplotlib import pyplot as plt
import numpy, os, csv

condict = {'nc':[0,9,'r'],'dn':[8,17,'g'],'no':[16,25,'m'],'ns':[24,33,'y']}

items = []
data = {}
filetypes = []

files = [i[2]for i in os.walk(os.getcwd())][0]
for file in files:
	if not file[3] == "_":
		files.remove(file)

for file in files:
	if file.endswith(".wav"):
		data[file[:3]]= []
subjects = [key for key in data]

for file in files:
	snum = file[:3]
	fset = [file]
	data[snum].append(fset)

global lines
lines = []
global mlines
mlines = []

for file in files:
	if file.endswith(".csv"):
		f = open(os.path.realpath(os.getcwd()+"/"+file))
		c = csv.reader(f, delimiter = "\t")
		firstline = True
		linenum = 0
		aimecheck = True
		for line in c:
			if firstline:
				firstline = False
			elif not line[0] == "_" and int(line[1])>10:
				if line[2][0] == 'nEm':
					linenum +=10
					aimecheck = False
				line[1] = linenum
				nline = [file[:3],int(file[4:6]),line]
				lines.append(nline)
				linenum += 1
		f.close()
	elif file.endswith(".means"):
		f = open(os.path.realpath(os.getcwd()+"/"+file))
		c = csv.reader(f, delimiter = "\t")
		firstline = True
		linenum = 5
		aimecheck = True
		for line in c:
			if firstline:
				firstline = False
			if not (line[0] == '_' or line[0] == 'rowLabel'):
				if aimecheck and line[0] == 'nEm':
					linenum += 10
					aimecheck = False
				mlines.append([file[0:3],int(file[4:6]),linenum,line[1],line[2]])
				linenum += 10
		f.close()

def subavf0(subject):
	count = 0
	f0total = 0
	for line in lines:
		if line[0] == subject:
			f0total += float(line[2][2])
			count +=1
	return float(f0total/count)

def subvar(subject):
	count = -1
	sumofsquares = 0
	for line in lines:
		if line[0] == subject:
			sumofsquares += (float(line[2][2])-subavf0(subject))**2
			count +=1
	return float(sumofsquares/count)

def substdev(subject):
	return subvar(subject)**(0.5)

def graph(subject):
	subset = []
	msubset = []
	plt.clf()
	plt.title(subject)
	for line in lines:
		if line[0] == subject:
			subset.append(line)
	st = substdev(subject)
	sa = subavf0(subject)
	for key in condict:
		condset,gcondset,mcondset,maxgcondset, mingcondset = [],[],[],[],[]
		for line in subset:
			if line[1] > condict[key][0] and line[1]<condict[key][1]:
				condset.append(line)
		for i in range(0,60):
			n = 0
			icount = []
			for line in condset:
				if line[2][1] == i:
					if not(float(line[2][2]) - sa)> 3*(st) or (float(line[2][2]) - sa) < -3*(st):
						n += (float(line[2][2])-sa)
						if line[1] not in icount:
							icount.append(line[1])
			gcondset.append([i,(n/len(icount))])
		plt.plot([i for i in range(0,60)],[i[1] for i in gcondset],condict[key][2])
		for line in mlines:
			if line[0] == subject:
				msubset.append(line)
		for line in msubset:
			if line[1] > condict[key][0] and line[1]<condict[key][1]:
				mcondset.append(line)
		for i in [(i*10)+5 for i in range(0,6)]:
			n = 0
			maxavg, minavg= 0,0
			for line in mcondset:
				if line[2] == i:
					if not(float(line[2]) - sa)> 3*(st) or (float(line[2]) -sa) < -3*(st):
						maxavg += float(line[3])-sa
						minavg += float(line[4])-sa
			maxgcondset.append([i,(maxavg/len(icount))])
			mingcondset.append([i,(minavg/len(icount))])
		plt.plot([(i*10)+5 for i in range(0,6)],[i[1] for i in maxgcondset],condict[key][2]+'o')
		plt.plot([(i*10)+5 for i in range(0,6)],[i[1] for i in mingcondset],condict[key][2]+'o')

s = '117'
# for s in subjects:
# 	graph(s)
# 	plt.savefig("graphs/%s.png" %s)
graph(s)
plt.show()
