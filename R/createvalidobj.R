# Function to create a validation object

createValidObj <- function(holdout.years, dataobj, response) {
    dataobj$data <- lapply(dataobj$data, function(x) {
        x[as.character(holdout.years), response] <- NA
        return(x)
    })
    return(dataobj)
} 
