count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
## Check that specific "cause" is allowed; else throw error
## Read "homicides.txt" data file
## Extract causes of death
## Return integer containing count of homicides for that cause
    homicides <- readLines("homicides.txt")
    # get a better view in google analytics
    # homicides <- as.data.frame(homicides)
    # library(googleVis)
    # gvt = gvisTable(homicides, options = list(showRowNumber = T, height = 800,width=1200))
    # plot(gvt)
    cause_arr <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
    if(!cause %in% cause_arr){
        stop(cause)
    } else {
        if(cause == "asphyxiation"){
            count <- length(grep("[Aa]sphyxiation", homicides))
        } else if(cause == "blunt force"){
            count <- length(grep("[Cc]ause: [Bb]lunt [Ff]orce", homicides))
        } else if(cause == "shooting"){
            count <- length(grep("[Cc]ause: [Ss]hooting", homicides))
        } else if(cause == "stabbing"){
            count <- length(grep("[Cc]ause: [Ss]tabbing", homicides))
        } else if(cause == "other"){
            count <- length(grep("[Cc]ause: [Oo]ther", homicides))
        } else { # cause == "unknown"
            count <- length(grep("[Cc]ause: [Uu]nknown", homicides))
        }
        count
    }
}