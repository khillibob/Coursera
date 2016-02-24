name = raw_input("Enter file:")
if len(name) < 1 : name = "mbox-short.txt"
handle = open(name)
dic=dict()
for line in handle:
	if line.startswith('From'):
		lst = line.split()
		#print lst
		if 'Jan' in lst:
			dic[lst[5].split(':')[0]] = dic.get(lst[5].split(':')[0],0)+1


lst = dic.items();
lst.sort()
for (k,v) in lst:
	print k,v		
