
#Testing lines
#datadir<-"~/Born To Code/R Programming/RProgramming_Project1/specdata"
#files<-list.files(path=datadir)
#setwd(datadir)

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        # So let's set the directory to our data folder
        
        #We first get a list of the files
        files<-list.files(path=directory)
        fno<-1:length(file)
        data.file<-data.frame(fno,files)
        #Creation of the data frame
        nobs<-rep(0,length(id))
        response<-data.frame(id,nobs)
        names(response)<-c("ids","nobs")
        #Loop for every id
        for (i in id){
                #We load the corresponding file
                temp.data<-read.csv(paste("~/specdata/",files[i],sep=""),header=TRUE)
                #We subset all the observation where both of variables are fulfilled (not NA)
                clean.data<-temp.data[which(!is.na(temp.data[,2]*temp.data[,3])),]
                #We pass the length of the clean dataset to our global data frame
                response[which(response$ids==i),2]<-dim(clean.data)[1]
        }
        names(response)<-c("id","nobs")
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        return(response)
}


