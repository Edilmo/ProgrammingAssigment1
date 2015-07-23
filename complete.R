complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        df <- data.frame(id=id, nobs=numeric(length = length(id)))
        
        fileIdNames <- dir(directory)
        
        for(fileIdName in fileIdNames){
                fileIdStr <- strsplit(fileIdName,".csv",fixed = TRUE)
                fileId <- as.numeric(fileIdStr)
                if(fileId %in% id){
                        tDF <- read.csv(file.path(directory,fileIdName))
                        
                        completeObservations <- which(!is.na(tDF[2]) & !is.na(tDF[3]))
                        for(i in which(df[,"id"]==fileId)){
                                df[i,"nobs"] <- length(completeObservations)
                        }
                }
        }
        
        df
}