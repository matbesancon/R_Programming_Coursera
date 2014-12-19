#Testing lines
#datadir<-"~/Born To Code/R Programming/RProgramming_Project1/specdata"
#files<-list.files(path=datadir)
#setwd(datadir)
source("complete.R")
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files<-list.files(path=directory)
        nobs<-rep(0,length(files))
        id<-1:length(files)

        completeObs<-complete(directory)
        #We create an empty vector for the correlation
        correlation<-rep(NA,length(files))
        for (i in 1:length(files)){
                if (completeObs$nobs[i]>threshold){
                        temp.file<-paste("~/specdata/",files[i],sep="")
                        temp.data<-read.csv(temp.file,header=TRUE)
                        correlation[i]<-cor(temp.data$sulfate,temp.data$nitrate,use="complete.obs")
                }
        }
        if (length(which(!is.na(correlation[])))>0){
                correlation.clean<-correlation[which(!is.na(correlation))]
                return(correlation)}
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
}
