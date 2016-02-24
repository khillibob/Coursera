pollutantmean <- function(directory,pollutant,id=1:332){
        ##setwd("..");
        setwd(file.path(getwd(), directory))
        createfilename <-function(name){
                fileName=""
                if(name<10){
                        fileName=paste("00",name,sep="")
                        fileName = paste(fileName,".csv",sep="")
                }
                else if(name<=99){
                        fileName=paste("0",name,sep="")
                        fileName = paste(fileName,".csv",sep="") 
                }
                else{
                        fileName = paste(name,".csv",sep="")   
                }
                fileName
        } 
        checkSulOrNit <-function(pollutant){
                choice=""
                if(pollutant=="sulfate"){
                        choice=sulfate
                }
                else{
                        choice=nitrate
                }
                choice
        }
        mean_vec = vector();
        for(i in id){
                fileName=createfilename(i)
                tab<-read.csv(fileName)
                if (pollutant == "sulfate") {poll=tab$sulfate}
                else {poll=tab$nitrate}
                poll<-poll[!is.na(poll)]
                mean_vec = append(mean_vec,poll)
        }
        setwd("..")
        mean(mean_vec);
} 

complete <- function(directory, id = 1:332) {
        ##setwd("..");
        setwd(file.path(getwd(), directory))
        
        
        createfilename <-function(name){
                fileName=""
                if(name<10){
                        fileName=paste("00",name,sep="")
                        fileName = paste(fileName,".csv",sep="")
                }
                else if(name<=99){
                        fileName=paste("0",name,sep="")
                        fileName = paste(fileName,".csv",sep="") 
                }
                else{
                        fileName = paste(name,".csv",sep="")   
                }
                fileName
        } 
        checkSulOrNit <-function(pollutant){
                choice=""
                if(pollutant=="sulfate"){
                        choice=sulfate
                }
                else{
                        choice=nitrate
                }
                choice
        }
        dataframe = NULL
        count<-0
        for(i in id){
                count=count+1
                
                fileName=createfilename(i)
                data<-read.csv(fileName,header = T, 
                               na.strings=c("NA","NaN", " "))
                data = na.omit(data) 
                data = as.matrix(data)
                dataframe = rbind(dataframe, c(i,nrow(data)))
                
        }
        
        setwd("..")  
        dataframe = data.frame(dataframe)  # from matix to data frame 
        names(dataframe) = c('id', 'nobs') # set the column names of the data frame
        return (dataframe) 
}
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        correlationVector=NULL
        setwd(file.path(getwd(), directory))
        createfilename <-function(name){
                fileName=""
                if(name<10){
                        fileName=paste("00",name,sep="")
                        fileName = paste(fileName,".csv",sep="")
                }
                else if(name<=99){
                        fileName=paste("0",name,sep="")
                        fileName = paste(fileName,".csv",sep="") 
                }
                else{
                        fileName = paste(name,".csv",sep="")   
                }
                fileName
        } 
        checkSulOrNit <-function(pollutant){
                choice=""
                if(pollutant=="sulfate"){
                        choice=sulfate
                }
                else{
                        choice=nitrate
                }
                choice
        }
        
        for(i in 1:332){
                fileName=createfilename(i)
                tab<-read.csv(fileName)
                data = na.omit(tab) 
                
                if (nrow(data) > threshold) {
                        correlationVector = c(correlationVector, cor(data[,2], data[,3]))
                }
        }
        setwd("..")
        return (correlationVector)
        
        
}
