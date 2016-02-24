##Question 1

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv";
download.file( fileURL, destfile = "./input_1_2.csv", method = "curl")
fileInput <- read.csv("input_1_2.csv")
fileInput
a<-fileInput$VAL==24
table(a)["TRUE"]

##Question2
fileInput$FES

##      Question 3
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx ";
download.file( fileURL, destfile = "./input_3.xlsx", method = "curl")
library(xlsx)
inputData <- read.xlsx("./input_3.xlsx", sheetIndex = 1, header = TRUE)
dat <- inputData[18:23,7:15]
sum(dat$Zip*dat$Ext,na.rm=T)
sum(dat$NA..5*dat$NA..10,na.rm=T)




##Q4

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml";
download.file( fileURL, destfile = "./input_4.xml", method = "curl")
library(XML)
doc <- xmlTreeParse("./input_4.xml", useInternal = TRUE);
rootNode <- xmlRoot(doc)
a<-xpathApply(rootNode,"//zipcode",xmlValue)
length(a[a==21231])
