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
    
        id <- as.integer(id)
        if(grep("specdata", directory) == 1){
            directory <- "/.../Computing for Data Analysis/Week2/specdata/"
        } # "..." is the directory in your computer; masked here for privacy
        len <- length(id)
        completedata <- rep(0, len)
        j = 1
        for(i in id){
            data <- getmonitor(i, directory)
            #sulfatelen <- dim(data[!is.na(data$sulfate), ])[1]
            #nitratelen <- dim(data[!is.na(data$nitrate), ])[1]
            idlen <- sum(complete.cases(data))
            #idlen <- min(sulfatelen, nitratelen)
            completedata[j] <- idlen
            j <- j + 1
        }
        ret <- data.frame(id = id, nobs = completedata) 
        ret
}
