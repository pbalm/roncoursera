source("complete.R")

corr <- function(directory, threshold = 0) {
    
    comp = complete(directory)[, "nobs"] > threshold
    monitors = seq_along(comp)[comp]
    
    sapply(getFilenames(directory, monitors), corrSingleMonitor)
}

corrSingleMonitor <- function(filename) {
    data <- read.csv(filename)
    compData <- data[complete.cases(data), ]
    cor(compData$sulfate, compData$nitrate)
}