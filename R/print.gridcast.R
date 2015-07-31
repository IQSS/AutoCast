#' @export
print.gridcast <- function(x, ...) {
    cat(paste("GridCast object with", x$validation$N.runs, "total YourCast runs\n\n"))
    cat("Model: ", x$aux$args.yourcast$model, "\n", sep = "")
    cat("Number of cross sections: ", length(x$aux$dataobj$data), "\n", sep = "")
    cat("Formula: ", deparse(x$aux$args.yourcast$formula), "\n\n", sep = "")
    cat("Observed period: ", x$aux$args.yourcast$sample.frame[1], "-", x$aux$args.yourcast$sample.frame[2], 
        "\n", sep = "")
    cat("Heldout times for validation: ", paste(x$validation$holdout.years, collapse = ","), 
        "\n", sep = "")
    cat("Forecast period: ", x$aux$args.yourcast$sample.frame[3], "-", x$aux$args.yourcast$sample.frame[4], 
        "\n", sep = "")
    cat("\n")
    invisible(x)
} 
