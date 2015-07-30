print.weightcast <- function(x, ...) {
    cat(paste("WeightCast object with", length(x), "chosen weight(s):\n\n"))
    cat(paste(names(x), collapse = "\n"))
    cat("\n\nSee 'help(forecast)' for instructions on how to run final forecasts using selected weights.\n")
    invisible(x)
} 
