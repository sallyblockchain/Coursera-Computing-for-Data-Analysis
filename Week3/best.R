best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate

    data <- read.csv(file="/.../data/outcome-of-care-measures.csv", colClasses = "character")
    # "..." is the directory in your computer; masked here for privacy
    reason <- c("heart attack", "heart failure", "pneumonia")
    data[, 11] <- as.numeric(data[, 11])
    data[, 23] <- as.numeric(data[, 23])
    data[, 17] <- as.numeric(data[, 17])

    if(!state %in% data$State){
        stop("invalid state")
    } else if(!outcome %in% reason){
        stop("invalid outcome")
    } else {
        if(outcome == "heart attack"){
            goal <- data[data$State == state, ]
            attack <- goal[, 11]
            #len <- dim(goal[!is.na(attack),])[1]
            #arr1 <- rep(0, len)
            #arr2 <- rep(0, len)
            #arr1 <- goal[!is.na(attack), 2]
            #arr2 <- sort.int(goal[, 11], index.return=T)
            #index <- arr2$ix[1]
            #hosp_name <- arr1[index]
            min <- min(attack, na.rm = T)
            index <- which(attack == min)
            hosp_name <- goal[, 2][index]
            # data[, 11]
        } else if(outcome == "heart failure"){
            goal <- data[data$State == state, ]
            failure <- goal[, 17]
            min <- min(failure, na.rm = T)
            index <- which(failure == min)
            hosp_name <- goal[, 2][index]
            # data[, 17]   
        } else { # "pneumonia"
            goal <- data[data$State == state, ]
            pneu <- goal[, 23]
            min <- min(pneu, na.rm = T)
            index <- which(pneu == min)
            hosp_name <- goal[, 2][index]
            # data[, 23]
        }
        hosp_name 
    }
}
