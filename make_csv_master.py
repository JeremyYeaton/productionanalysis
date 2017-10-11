import numpy, os, csv

items = []
data = {}
filetypes = []

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
	snum = file[:3]
	fset = [file]
	data[snum].append(fset)

global lines
lines = []
for file in files:
	if file.endswith(".csv"):
		f = open(os.path.realpath(p+"/"+file))
		c = csv.reader(f, delimiter = "\t")
		firstline = True
		aimecheck = True
		linenum = 0
		for line in c:
			if firstline:
				firstline = False
			elif not line[0] == "_" and int(line[1])>10:
				if aimecheck and line[0] == "nEm":
					aimecheck = False
					linenum += 10
				line[1] = linenum
				nline = [str(file[:3]),file[4:6],str(line[0]),str(line[1]),"".join([str(line[2]),"\n"])]
				lines.append(nline)
				linenum += 1
		f.close()

f = open("master.csv",'w')
f.write(",".join(["subj","trial","syll","series","raw_f0\n"]))
for line in lines:
	f.write(",".join(line))
f.close()