rankhospital<-function(state,outcome,num){
        
        possible.outcome<-c("heart attack","heart failure","pneumonia")
        if (outcome==possible.outcome[1]){
                index.mort<-11
        } else if(outcome==possible.outcome[2]){
                index.mort<-17
        } else if(outcome==possible.outcome[3]){
                index.mort<-23
        }
        state.list<-unique(raw.data$State)
        
        ## Check that state and outcome are valid
        if ((state%in%state.list)==FALSE){
                stop("invalid state")} 
        if((outcome%in%possible.outcome)==FALSE) {
                stop("invalid outcome") } 

        ## Read outcome data
        raw.data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        data<-raw.data[,c(2,7,index.mort)]
        selected.data<-data[which(data$State==state),]
        selected.data[,3]<-as.double(selected.data[,3])
        selected.data<-selected.data[which(!is.na(selected.data[,3])),]
        names(selected.data)<-c("name","State","Outcome")
        selected.data<-selected.data[order(-selected.data$Outcome,selected.data$name),]
        ranking<-selected.data$name
        if (is.numeric(num)==TRUE & num<=length(ranking) ){
                return(ranking[num])
        } else if (num=="worst"){
                return(ranking[which.max(selected.data$Outcome)])
        } else if (num=="best"){
                return(ranking[which.min(selected.data$Outcome)])
        } else {return(NA)}        
                
        
}