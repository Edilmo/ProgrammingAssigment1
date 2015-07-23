corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        corrVector <- numeric()
        
        fileIdNames <- dir(directory)
        
        for(fileIdName in fileIdNames){
                fileIdStr <- strsplit(fileIdName,".csv",fixed = TRUE)
                fileId <- as.numeric(fileIdStr)

                tDF <- read.csv(file.path(directory,fileIdName))
                
                completeObservations <- which(!is.na(tDF[2]) & !is.na(tDF[3]))
                
                if(length(completeObservations) > threshold){
                        sulfate <- tDF[completeObservations,"sulfate"] 
                        nitrate <- tDF[completeObservations,"nitrate"] 
                        corrVector <- c(corrVector,cor(sulfate,nitrate))
                }
        }
        
        corrVector
}