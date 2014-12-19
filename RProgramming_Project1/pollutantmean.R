
#Testing lines
#maindir<-"~/Born To Code/R Programming/RProgramming_Project1"
datadir<-"~/Born To Code/R Programming/RProgramming_Project1/specdata"
#files<-list.files(path=datadir)
#pollutant<-"sulfate"

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files<-list.files(path=directory)
        #setwd(directory)
        files.path<-paste("~/specdata/",files,sep="")
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        #Exctracting the data from all CSV to a dataset
        raw.data <- do.call(rbind, lapply(files.path, read.csv))
        
        #Aggregating the data with the mean and per given id
        index.poll<-which(names(raw.data)==pollutant)
        raw.data<-raw.data[which(raw.data$ID%in%id),]
        res<-mean(raw.data[,index.poll],na.rm=TRUE)
        #Just keeping the variable of interest
                 
        return(res)
}




