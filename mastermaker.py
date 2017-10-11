import os, csv

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

f = open("master.csv",'w')

# for line in lines:


f.close()