
import urllib
from BeautifulSoup import *

url = raw_input('Enter - ')
html = urllib.urlopen(url).read()

soup = BeautifulSoup(html)

# Retrieve all of the anchor tags
tags = soup('span')
sum1=0
for tag in tags:
    sum1=sum1+int(tag.contents[0])
print sum1

