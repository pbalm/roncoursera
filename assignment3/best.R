best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    validateState(state, data)
    columnNumber <- validateOutcome(outcome, data)
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    # Get the hospital name and outcome columns for this state
    bystate <- split(data[c(2, colNumber)], data$State)
    thisstate <- bystate[state][[1]]
    
    # convert the outcome column to numeric
    thisstate[,2] <- as.numeric(thisstate[,2])
    
    # exclude NAs
    x <- thisstate[!is.na(thisstate[,2]), ]
    
    # Find the min of the 2nd col, get the row index and get
    # hospital name
    x[match(min(x[,2]), x[,2]), 1]
}

validateState <- function(state, data) {
    states = levels(as.factor(data$State))
    if (!(state %in% states)) {
        stop("invalid state")
    }
}

validateOutcome <- function(outcome, data) {
    allowedOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% allowedOutcomes)) {
        stop("invalid outcome")
    }
    
    colNumbers <- c(11, 17, 23)
    colNumber <- colNumbers[match(outcome, allowedOutcomes)]
    # return column number for this outcome
    colNumber
}