
# Fist function "best", takes the abbreviated name of state 
# and the outcome in argument

best<-function(state,outcome){
        possible.outcome<-c("heart attack","heart failure","pneumonia")
        ## Read outcome data
        raw.data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        # generating a list of states
        state.list<-unique(raw.data$State)
        
        ## Check that state and outcome are valid
        if ((state%in%state.list)==FALSE){
                stop("invalid state")
        }else if((outcome%in%possible.outcome)==FALSE) {
                stop("invalid outcome")
        } #Eventually, if everything is good, we run the function
        else{
                # we take only the data from the state we want
                selected.data<-raw.data[which(raw.data$State==state),]
                
                #we take a column index refering to the mortality rate for our outcome
                if (outcome==possible.outcome[1]){
                        index.mort<-11
                } else if(outcome==possible.outcome[2]){
                        index.mort<-17
                } else if(outcome==possible.outcome[3]){
                        index.mort<-23
                }
                #We turn our data of interest into real numbers
                selected.data[,index.mort]<-as.double(selected.data[,index.mort])
                #we get rid of the NA for our interesting column
                selected.data<-selected.data[which(!is.na(selected.data[,index.mort])),]
                index.mini<-which(selected.data[,index.mort]==min(selected.data[,index.mort]),)
                return(selected.data$Hospital.Name[index.mini])
        }
                
        }
        ## Return hospital name in that state with lowest 30-day death rate
