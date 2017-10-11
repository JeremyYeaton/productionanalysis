import numpy, os, csv

items = []
data = {}

condict = {'nc':[0,9,'r'],'dn':[8,17,'g'],'negob':[16,25,'m'],'negsub':[24,33,'y']}

p = os.path.realpath(os.getcwd()+"/processed_data")

files = [i[2]for i in os.walk(p)][0]
for file in files:
	if not file[3] == "_":
		files.remove(file)

for file in files:
	if file.endswith(".wav"):
		data[file[:3]]= []
subjects = [key for key in data]

for file in files:
	subnum = file[:3]
	fileset = [file]
	data[subnum].append(fileset)

global lines
lines = []
for file in files:
	if file.endswith(".csv"):
		f = open(os.path.realpath(p+"/"+file))
		c = csv.reader(f, delimiter = "\t")
		firstline = True
		aimecheck = True
		linenum = 0
		for key in condict:
			if int(file[4:6]) > condict[key][0] and int(file[4:6]) < condict[key][1]:
				condition = key
		for line in c:
			if firstline:
				firstline = False
			elif not line[0] == "_" and int(line[1])>10:
				if aimecheck and line[0] == "nEm":
					aimecheck = False
					linenum += 10
				line[1] = linenum
				nline = [file[:6],str(file[:3]),file[4:6],str(line[0]),str(line[1]),str(line[2]),"%s\n" %condition]
				lines.append(nline)
				linenum += 1
		f.close()

f = open("master.csv",'w')
f.write(",".join(["obj_id","subj","trial","syll","series","raw_f0","condition\n"]))
for line in lines:
	f.write(",".join(line))
f.close()