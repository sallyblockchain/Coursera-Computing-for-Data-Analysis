helper <- function(data, outcome, num){
    rank <- data[, 2][order(outcome, data[, 2])[num]]
    rank
}

rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
    data <- read.csv(file="/.../data/outcome-of-care-measures.csv", colClasses = "character")
    # "..." is the directory in your computer; masked here for privacy
    reason <- c("heart attack", "heart failure", "pneumonia")
    goal <- data[data$State == state, ]
    attack <- as.numeric(goal[, 11])
    failure <- as.numeric(goal[, 17])
    pneumonia <- as.numeric(goal[, 23])
    
    if(!state %in% data$State){
        stop("invalid state")
    } else if(!outcome %in% reason){
        stop("invalid outcome")
    } else {
        if(num == "best"){
            rank <- best(state, outcome)
        } else{ # num != "best"
            if(outcome == "heart attack"){
                len <- dim(goal[!is.na(attack),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, attack, len)
                } else{
                    rank <- helper(goal, attack, num)
                }
                # data[, 11]
            } else if(outcome == "heart failure"){
                len <- dim(goal[!is.na(failure),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, failure, len)
                } else{
                    rank <- helper(goal, failure, num)
                }
                # data[, 17]
            }else{ # "pneumonia"
                len <- dim(goal[!is.na(pneumonia),])[1]
                if(num != "worst" && num > len){
                    rank <- NA
                } else if(num == "worst"){
                    rank <- helper(goal, pneumonia, len)
                } else{
                    rank <- helper(goal, pneumonia, num)
                }
                # data[, 23]
            }            
        }
    }
    rank
}
