name = raw_input("Enter file:")
if len(name) < 1 : name = "mbox-short.txt"
handle = open(name)
m = 0;
word=''
lst=list()
dictionary= dict()
for line in handle:
    #print dictionary
    if line.startswith('From'):
        lst = line.split();
        #print lst
	if 'Jan' in lst:
		dictionary[lst[1]]= dictionary.get(lst[1],0)+1
            	if dictionary[lst[1]]>m:
                	m = dictionary[lst[1]];
                	word = lst[1];
print word,m
