complete <- function(directory, id = 1:332) {
    nobs = sapply(getFilenames(directory, id), countCompleteCases)
    data.frame(id, nobs)
}

countCompleteCases <- function(filename) {
    # complete.cases returns a vector of logicals. By summing it,
    # we can count the number of TRUEs.
    sum(complete.cases(read.csv(filename)))
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
