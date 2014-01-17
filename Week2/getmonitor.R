getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
        id <- as.integer(id)
        if(grep("specdata", directory) == 1){
            directory <- "/.../Computing for Data Analysis/Week2/specdata/"
        } # "..." is the directory in your computer; masked here for privacy
        if(id < 10){
            newID <- paste("00", id, sep = "")
            fileID <- paste(newID, ".csv", sep = "") # 001.csv
            fileName <- paste(directory, fileID, sep = "")
            data <- read.csv(file = fileName)
        } else if(id >= 10 & id <= 99){
            newID <- paste("0", id, sep = "")
            fileID <- paste(newID, ".csv", sep = "")
            fileName <- paste(directory, fileID, sep = "")
            data <- read.csv(file = fileName)
        } else{
            fileID <- paste(id, ".csv", sep = "")
            fileName <- paste(directory, fileID, sep = "")
            data <- read.csv(file = fileName)
        }
        
        if(summarize == TRUE){
            print(summary(data))
            data
        } else{
            data
        } 
}
