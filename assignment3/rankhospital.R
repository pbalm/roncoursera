source("best.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validateState(state, data)
    colNumber <- validateOutcome(outcome, data)
    
    # Get the hospital name and outcome columns for this state
    bystate <- split(data[c(2, colNumber)], data$State)
    thisstate <- bystate[state][[1]]
    
    # convert the outcome column to numeric
    thisstate[,2] <- as.numeric(thisstate[,2])
    
    # exclude NAs
    x <- thisstate[!is.na(thisstate[,2]), ]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    orderedIndex <- order(x[,2], x[,1])
    
    index <- if (num == "best") {
        orderedIndex[1]
    } else if (num == "worst") {
        orderedIndex[length(orderedIndex)]
    } else {
        orderedIndex[num]
    }
    
    x[index, 1]
}
