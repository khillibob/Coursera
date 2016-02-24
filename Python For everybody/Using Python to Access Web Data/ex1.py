import re
f = open('ex1_data').read();
numbers = re.findall('[0-9]+',f);
sum1=0
for inp in numbers:
	sum1 =sum1+ int(inp);
print sum1;
