source("pollutantmean.R")

complete <- function(directory, id = 1:332) {
    nobs = sapply(getFilenames(directory, id), countCompleteCases)
    data.frame(id, nobs)
}

countCompleteCases <- function(filename) {
    # complete.cases returns a vector of logicals. By summing it,
    # we can count the number of TRUEs.
    sum(complete.cases(read.csv(filename)))
}
