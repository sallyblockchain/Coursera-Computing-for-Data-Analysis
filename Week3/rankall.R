helper <- function(data, outcome, num){
    hospital <- data[, 2][order(outcome, data[, 2])[num]]
    hospital
}

rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

    data <- read.csv(file="/.../data/outcome-of-care-measures.csv", colClasses = "character")
    # "..." is the directory in your computer; masked here for privacy
    reason <- c("heart attack", "heart failure", "pneumonia")
    state_arr <- sort(unique(data$State))
    arr_len <- length(state_arr)
    hospital <- rep("", arr_len)
    
    if(!outcome %in% reason){
        stop("invalid outcome")
    } else {
        for(i in 1:arr_len){
            goal <- data[data$State == state_arr[i], ] # loop for each state
            if(outcome == "heart attack"){
                attack <- as.numeric(goal[, 11])   
                len <- dim(goal[!is.na(attack),])[1]
                if(num == "best"){
                    hospital[i] <- helper(goal, attack, 1)
                } else if(num == "worst"){
                    hospital[i] <- helper(goal, attack, len)
                } else if(num > len){
                    hospital[i] <- NA
                } else{
                    hospital[i] <- helper(goal, attack, num)
                }          
            }

            else if(outcome == "heart failure" ){ # Attention here!
                failure <- as.numeric(goal[, 17])   
                len <- dim(goal[!is.na(failure),])[1]
                if(num == "best"){
                    hospital[i] <- helper(goal, failure, 1)
                    #hospital[i] <- best(state_arr[i], "heart failure")
                } else if(num == "worst"){
                    hospital[i] <- helper(goal, failure, len)
                } else if(num > len){
                    hospital[i] <- NA
                } else{
                    hospital[i] <- helper(goal, failure, num)
                } 
            }

            else{
                pneumonia <- as.numeric(goal[, 23])
                len <- dim(goal[!is.na(pneumonia),])[1]
                if(num == "best"){
                    #hospital[i] <- best(state_arr[i], "pneumonia")
                    hospital[i] <- helper(goal, pneumonia, 1)
                } else if(num == "worst"){
                    hospital[i] <- helper(goal, pneumonia, len)
                } else if(num > len){
                    hospital[i] <- NA
                } else{
                    hospital[i] <- helper(goal, pneumonia, num)   
                } 
            }  
        } # end of the for loop
        
        df <- data.frame(hospital = hospital, state = state_arr)
        df
    }
}