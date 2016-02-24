
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
                
                
                tab_com[,4]<-as.numeric(levels(tab_com[,4])[tab_com[,4]]);
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
        else if(outcome==out[3]){
                
                
                tab_com[,5]<-as.numeric(levels(tab_com[,5])[tab_com[,5]]);
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
        
        df <- data.frame(hospital=result[,1], state=result[,2])
        result1 <- df
        return(result1)
}

