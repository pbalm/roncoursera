pollutantmean <- function(directory, pollutant, id = 1:332) {
    #cat("pollutantmean", pollutant, id[1], id[length(id)])
    filenames <- getFilenames(directory, id)
    data <- sapply(filenames, pollutantForFile, pollutant)
    # calculate the mean across all values at once, not
    # per file, followed by the mean of the files
    mean(unlist(data), na.rm=TRUE)
}

## data is a list of the columns per file, of varying length
## this method returns all this data as one single list
onebiglist <- function(data) {
    out <- NULL
    sapply(data, function(x, out) out <- c(out, x), out)
    out
}

pollutantForFile <- function(filename, pollutant) {
    # Read the file, and get the right column
    read.csv(filename)[, pollutant]
}

## Given a directory and a vector of IDs, return a vector of filenames
getFilenames <- function(directory, id) {
    sapply(id, getSingleFilename, directory )
}

## Given a number and a directory, get the complete name for a single file
getSingleFilename <- function(id, directory) {
    
    # get name=001 given id=1, 011 given 11, etc
    name <- id
    while(id < 100) {
        name <- paste("0", name, sep="")
        id <- id*10; 
    }

    # generate the filename
    paste(directory, "/", name, ".csv", sep="")
}

