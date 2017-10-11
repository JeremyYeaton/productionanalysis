import os, csv
from matplotlib import pyplot as plt

items = []
data = {}
filetypes = []
condict = {'nc':[0,9,'r'],'dn':[8,17,'g'],'no':[16,25,'m'],'ns':[24,33,'y']}

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

for file in files:
	if file.endswith(".csv"):
		f = open(os.path.realpath(os.getcwd()+"/"+file))
		c = csv.reader(f, delimiter = "\t")
		firstline = True
		linenum = 0
		for line in c:
			if firstline:
				firstline = False
			elif not line[0] == "_" and int(line[1])>10:
				line[1] = linenum
				nline = [file[:3],int(file[4:6]),line]
				lines.append(nline)
				linenum += 1
		f.close()
# def subavf0(subject):
# 	count = 0
# 	f0total = 0
# 	for line in lines:
# 		if line[0] == subject:
# 			f0total += float(line[2][2])
# 			count +=1
# 	return float(f0total/count)


# def graphcond(cond,subject):
# 	graphset = []
# 	for i in range(0,60):
# 		n = 0
# 		icount = []
# 		for line in lines:
# 			if line[1] not in icount:
# 				icount.append(line[1])
# 			if line[1] > condict[cond][0] and line[1]<condict[cond][1] and line[0] == subject and line[2][1] == i:
# 				n += (float(line[2][2])-subavf0(subject))
# 		graphset.append([i,(n/len(icount))])
# 	plt.plot([i for i in range(0,60)],[i[1] for i in graphset],condict[cond][2])
# 	plt.title(subject)



# for s in subjects:
# 	plt.clf()
# 	for key in condict:
# 		graphcond(key,s)
# 	plt.savefig("graphs/%s.png" %s)