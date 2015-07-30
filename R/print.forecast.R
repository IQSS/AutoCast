print.forecast <- function(x, ...) {
    cat(paste("AutoCast forecast\n\n"))
    cat("Model: ", x$aux$args.yourcast$model, "\n", sep = "")
    cat("Number of cross sections: ", length(x$aux$dataobj$data), "\n", sep = "")
    cat("Formula: ", deparse(x$aux$args.yourcast$formula), "\n\n", sep = "")
    cat("Observed period: ", x$aux$args.yourcast$sample.frame[1], "-", x$aux$args.yourcast$sample.frame[2], 
        "\n", sep = "")
    cat("Forecast period: ", x$aux$args.yourcast$sample.frame[3], "-", x$aux$args.yourcast$sample.frame[4], 
        "\n\n", sep = "")
    cat("See 'help(plot.forecast)' for details on how to visualize forecasts.\n")
    invisible(x)
} 
