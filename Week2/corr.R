corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
    
        if(grep("specdata", directory) == 1){
            directory <- "/.../Computing for Data Analysis/Week2/specdata/"
        } # "..." is the directory in your computer; masked here for privacy
        completetable <- complete("specdata", 1:332)
        nobs <- completetable$nobs
        ids <- completetable$id[nobs > threshold]
        len <- length(ids)
        cr <- rep(0, len)
        j = 1
        for(i in ids){
            data <- getmonitor(i, directory)
            cr[j] <- cor(data$sulfate, data$nitrate, use="complete.obs")
            j = j + 1
        }
        cr     
}
