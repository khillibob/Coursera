#setwd("D:/Coursera/R_Programming/Week4")
setwd("~/Documents/Coursera/R_Programming/Week4")
best <- function(state, outcome) {
        ## Read outcome data
        #state='TX'
        tab <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #tab<-tab[complete.cases(tab[,]),];
        
        ## Check that state and outcome are valid
        checkState = tab[,7];
        checkState = checkState[state==checkState]
        if(length(checkState)<1){
                stop("invalid state");
        }
        
        # outcome <-"heart attack"
        out<-c("heart attack", "heart failure",  "pneumonia");
        if(length(out[outcome==out])<1){
                stop("invalid outcome");
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        max_name<-""
        tab <- tab[state==tab[,7],];
        if(outcome==out[2]){
                
                tab_num <-tab;
                tab_num[,17]<-as.numeric(unlist(tab[17]))
                tab1<-tab_num[complete.cases(tab_num[,17]),];
                #print(a)
                #print(length(tab))
                # names <- tab1[a==tab[,12],]
                
                #names<-names[names$State==state,]
                minval<-min(tab1[,17]);
                min_tab<-tab1[(tab1[,17]==minval),]
                names<-min_tab$Hospital.Name
                #print(names)
                max_name<-min(names)
                max_name<-max_name[1]
                
        }
        else if(outcome==out[1]){
                print('1')
                tab_num <-tab;
                tab_num[,11]<-as.numeric(unlist(tab[11]))
                tab1<-tab_num[complete.cases(tab_num[11]),];
                #print(a)
                #print(length(tab))
                # names <- tab1[a==tab[,12],]
                print('1_1')
                #names<-names[names$State==state,]
                minval<-min(tab1[,11]);
                min_tab<-tab1[(tab1[,11]==minval),]
                names<-min_tab$Hospital.Name
                #print(names)
                print('1_2')
                
                max_name<-min(names)
                max_name<-max_name[1]
                
        }
        else if(outcome==out[3]){
                
                tab_num <-tab;
                tab_num[,23]<-as.numeric(unlist(tab[23]))
                tab1<-tab_num[complete.cases(tab_num[,23]),];
                #print(a)
                #print(length(tab))
                # names <- tab1[a==tab[,12],]
                
                #names<-names[names$State==state,]
                minval<-min(tab1[,23]);
                min_tab<-tab1[(tab1[,23]==minval),]
                names<-min_tab$Hospital.Name
                #print(names)
                max_name<-min(names)
                max_name<-max_name[1]
                
        }
        max_name
}

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        #state <- 'TX';
        #outcome = "pneumonia";
        tab <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #tab<-tab[complete.cases(tab[,]),];
        
        ## Check that state and outcome are valid
        checkState = tab[,7];
        checkState = checkState[state==checkState]
        if(length(checkState)<1){
                stop("invalid state");
        }
        
        # outcome <-"heart attack"
        out<-c("heart attack", "heart failure",  "pneumonia");
        if(length(out[outcome==out])<1){
                stop("invalid outcome");
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        max_name<-NA
        tab <- tab[state==tab[,7],];
        if(outcome==out[2]){
                
                tab_num <-tab;
                tab_num[,17]<-as.numeric(unlist(tab[,17]))
                tab1<-tab_num[complete.cases(tab_num),];
                
                new_tab<-tab1[with(tab1, order(tab1[,17], tab1[,2])), ]
                if(num == 'worst'){
                        max_name = new_tab[nrow(new_tab),2]
                }
                else if(num == 'best'){
                        max_name = new_tab[1,2]
                }
                else{
                        max_name = new_tab[num,2]
                }
        }
        else if(outcome==out[1]){
                tab_num <-tab;
                tab_num[,11]<-as.numeric(unlist(tab[,11]))
                tab1<-tab_num[complete.cases(tab_num),];
                
                new_tab<-tab1[with(tab1, order(tab1[,11], tab1[,2])), ]
                if(num == 'worst'){
                        max_name = new_tab[nrow(new_tab),2]
                }
                else if(num == 'best'){
                        max_name = new_tab[1,2]
                }
                else{
                        max_name = new_tab[num,2]
                }
        }
        else if(outcome==out[3]){
                tab_num <-tab;
                tab_num[,23]<-as.numeric(unlist(tab[,23]))
                tab1<-tab_num[complete.cases(tab_num),];
            ##    num = 10;
                new_tab<-tab1[with(tab1, order(tab1[,23], tab1[,2])), ];
                if(num == 'worst'){
                        max_name = new_tab[nrow(new_tab),2]
                }
                else if(num == 'best'){
                        max_name = new_tab[1,2]
                }
                else{
                        max_name = new_tab[num,2]
                }
        }
        max_name
        
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        tab <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        out<-c("heart attack", "heart failure",  "pneumonia");
        if(length(out[outcome==out])<1){
                stop("invalid outcome");
        }
        
        
        res_name<-NULL;
        
        ## For each state, find the hospital of the given rank
        new_tab<-data.frame(tab$Hospital.Name,tab$State,tab[,11],tab[,17],tab[,23])
        
        colnames(new_tab) <- c("HospitalName","State","HADeath","HFDeath","PNDeath")
        tab_com <- data.frame(new_tab[complete.cases(new_tab),],stringsAsFactors=FALSE);
        ## states<-unique(tab_com$State);
        result <- data.frame();
        if(outcome==out[1]){
                
                tab_com[,3]<-as.numeric(levels(tab_com[,3])[tab_com[,3]]);
                tab_com[,1]<-as.character(tab_com[,1]) ;
                newdata <- tab_com[order(tab_com$State,tab_com$HADeath,tab_com$HospitalName),];
                as.character(newdata[,1]) 
                states<-as.character(unique(newdata$State));
                if(num == 'worst'){
                        newdata <- newdata[!(newdata$HADeath=='Not Available'),];
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                print(state)
                                state_flag <- newdata[newdata$State==state,];
                                num = nrow(state_flag);
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                i = i+1;
                        }
                }
                else if(num == 'best'){
                        
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                
                                num = 1;
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                
                                i = i+1;
                        }
                }
                
                else{
                        
                        i<-1;                
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                #  i=1
                                # result[i,] = c(state_flag[num,state_flag$HospitalName],state_flag[num,state_flag$State]);
                                if(num>(length(state_flag$HospitalName))){
                                        result$x[i] <- NA;
                                        
                                        #   res_name[i] = state_flag[num,1];
                                }
                                else{
                                        result$x[i] <- state_flag[num,1];
                                        
                                        #     res_name[i] = state_flag[num,1];
                                }
                                
                                result$y[i] <- state;
                                i = i+1;
                        }
                }
        }
        
        
        else if(outcome==out[2]){
                tab_com[,1]<-as.character(tab_com[,1]) ;
                tab_com[,4]<- as.numeric(levels(tab_com[,4])[tab_com[,4]]);
                newdata <- tab_com[order(tab_com$State,tab_com$HFDeath,tab_com$HospitalName),];
                
                
                states<-as.character(unique(newdata$State));
                if(num == 'worst'){
                        newdata <- newdata[!(newdata$HFDeath=='Not Available'),];
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                print(state)
                                state_flag <- newdata[newdata$State==state,];
                                num = nrow(state_flag);
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                i = i+1;
                        }
                }
                else if(num == 'best'){
                        
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                
                                num = 1;
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                
                                i = i+1;
                        }
                }
                
                else{
                        
                        i<-1;                
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                #  i=1
                                # result[i,] = c(state_flag[num,state_flag$HospitalName],state_flag[num,state_flag$State]);
                                if(num>(length(state_flag$HospitalName))){
                                        result$x[i] <- NA;
                                        
                                        #   res_name[i] = state_flag[num,1];
                                }
                                else{
                                        result$x[i] <- state_flag[num,1];
                                        
                                        #     res_name[i] = state_flag[num,1];
                                }
                                
                                result$y[i] <- state;
                                i = i+1;
                        }
                }
        }
        else if(outcome==out[3]){
                
                #  tab_com[,5] <-as.numeric(tab_com[,5]) ;
                tab_com[,1]<-as.character(tab_com[,1]) ;
                tab_com[,5]<-as.numeric(levels(tab_com[,5])[tab_com[,5]])
                #  as.character(tab_com[,1]) ;
                newdata <- tab_com[order(tab_com$State,tab_com$PNDeath,tab_com$HospitalName),];
                newdata[,1] <-as.character(newdata[,1]) 
                states<-as.character(unique(newdata$State));
                if(num == 'worst'){
                        newdata <- newdata[!(newdata$PNDeath=='Not Available'),];
                        newdata <- newdata[!is.na(newdata$PNDeath),];
                        
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                       # state="AK"
                        for(state in states){
                                print(state)
                                state_flag <- newdata[newdata$State==state,];
                                num = nrow(state_flag);
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                i = i+1;
                        }
                }
                else if(num == 'best'){
                        
                        i<-1;
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                
                                num = 1;
                                result$x[i] = state_flag[num,1];
                                result$y[i] = state;
                                
                                i = i+1;
                        }
                }
                
                else{
                        
                        i<-1;                
                        result <- data.frame(x = character(54), y = character(54), stringsAsFactors = FALSE);
                        
                        for(state in states){
                                state_flag <- newdata[newdata$State==state,];
                                #  i=1
                                # result[i,] = c(state_flag[num,state_flag$HospitalName],state_flag[num,state_flag$State]);
                                if(num>(length(state_flag$HospitalName))){
                                        result$x[i] <- NA;
                                        
                                        #   res_name[i] = state_flag[num,1];
                                }
                                else{
                                        result$x[i] <- state_flag[num,1];
                                }
                                result$y[i] <- state;
                                i = i+1;
                        }
                }
        }
        df <- data.frame(hospital=result[,1], state=result[,2])
        result1 <- df
        return(result1)
}


dispHist <- function(){
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
        outcome[, 11] <- as.numeric(outcome[, 11]);
        hist(outcome[, 11]);
        
}
