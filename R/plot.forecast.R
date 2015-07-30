#' @title Plot AutoCast forecasts.
#'
#' @description
#' Create plots of age and time profiles of forecasts.
#'
#' @param x Object of class \code{"forecast"}. 
#' @param type Character. Type of plot: "age", "time", or "agetime". Default: "agetime".
#' @param title Character. Title to be used for plot. Default: \code{NULL}.
#' @param subtitle Character. Subtitle to be used for plot. Default: \code{NULL}.
#' @param ageprofile.opts List.
#' @param timeprofile.opts List.
#' @param print Character. Must be either "device" or "pdf". Default: "device".
#' @param print.opts List.

plot.forecast <- function(x, type = "agetime", title = NULL, subtitle = NULL, ageprofile.opts = list(), 
    timeprofile.opts = list(), print = "device", print.opts = list()) {
    # check for class
    if (class(x) != "forecast") {
        stop("'x' must be an object of class 'forecast'.", call. = F)
    }
    
    # checks
    plottype <- match(type, c("agetime", "age", "time"))
    if (is.na(plottype)) {
        stop("'type' argument must be 'agetime', 'age', or 'time'.", call. = F)
    }
    if (plottype == 1) {
        plot.ageprofile <- T
        plot.timeprofile <- T
        default_width <- 10
    } else if (plottype == 2) {
        plot.ageprofile <- T
        plot.timeprofile <- F
        default_width <- 5
    } else {
        plot.ageprofile <- F
        plot.timeprofile <- T
        default_width <- 5
    }
    
    
    if (!is.null(title) & !is.character(title)) {
        stop("'title' must be of class 'character' or NULL.", call. = F)
    }
    if (!is.null(subtitle) & !is.character(subtitle)) {
        stop("'subtitle' must be of class 'character' or NULL.", call. = F)
    }
    
    # check that print is either device or pdf
    if (is.na(match(print, c("device", "pdf")))) 
        stop("'print' argument must be either 'device' or 'pdf'.", call. = F)
    
    # setup default plot options
    age.options <- list(xlab = "Age", ylab = "Data and Forecasts", insample.obs = FALSE, insample.forecast = TRUE, 
        uncertainty = FALSE)
    time.options <- list(xlab = "Time", ylab = "Data and Forecasts", insample.obs = TRUE, 
        insample.forecast = TRUE, uncertainty = TRUE)
    print.options <- list(height = 5, width = default_width, filename = substring(tempfile(pattern="forecast_", tmpdir="", fileext=".pdf"),2), path = NULL)
    
    # get names of all available options
    age.option.names <- names(age.options)
    time.option.names <- names(time.options)
    print.option.names <- names(print.options)
    
    # update default options with user options
    age.options[(nam.age.opts <- names(ageprofile.opts))] <- ageprofile.opts
    time.options[(nam.time.opts <- names(timeprofile.opts))] <- timeprofile.opts
    print.options[(nam.print.opts <- names(print.opts))] <- print.opts
    
    # return warnings if option names do not match
    if (length(noNames <- nam.age.opts[!nam.age.opts %in% age.option.names])) 
        warning("Unknown names in age plot options: ", paste(noNames, collapse = ", "), call. = FALSE)
    if (length(noNames <- nam.time.opts[!nam.time.opts %in% time.option.names])) 
        warning("Unknown names in time plot options: ", paste(noNames, collapse = ", "), call. = FALSE)
    if (length(noNamesPrint <- nam.print.opts[!nam.print.opts %in% print.option.names])) 
        warning("Unknown names in plot print options: ", paste(noNamesPrint, collapse = ", "), 
            call. = FALSE)
    
    
    # create data frame with both observed & forecast data
    a <- array(data = NA, dim = c(length(x$aux$ages), length(x$aux$times), 4), dimnames = list(age = x$aux$ages, 
        time = x$aux$times, variable = c("y", "yhat", "yhat.lower", "yhat.upper")))
    a[rownames(x$y), colnames(x$y), "y"] <- x$y
    a[rownames(x$yhat), colnames(x$yhat), "yhat"] <- x$yhat
    a[rownames(x$yhat.lower), colnames(x$yhat.lower), "yhat.lower"] <- x$yhat.lower
    a[rownames(x$yhat.upper), colnames(x$yhat.upper), "yhat.upper"] <- x$yhat.upper
    a <- dcast(melt(a), age + time ~ variable)
    attr(a, "sample.frame") <- x$aux$args.yourcast$sample.frame
    
    # set up plots
    if (plot.ageprofile) {
        age.options$d <- a
        ap <- do.call(ageplot, age.options)
    } else {
        ap <- NULL
    }
    if (plot.timeprofile) {
        time.options$d <- a
        tp <- do.call(timeplot, time.options)
    } else {
        tp <- NULL
    }
    
    plot.list <- list(ap, tp)
    plot.list <- plot.list[!sapply(plot.list, function(x) is.null(x))]
    grid.args <- c(plot.list, list(nrow = 1, ncol = length(plot.list), top = title, bottom = subtitle))
    
    if (print == "device") {
        dev.new(height = print.options$height, width = print.options$width)
    }
    if (print == "pdf") {
    	if(!is.null(print.options$path)){filename <- file.path(print.options$path, print.options$filename)} else {filename <- print.options$filename}
        pdf(file = filename, height = print.options$height, width = print.options$width)
    }
    do.call(grid.arrange, grid.args)
    if (print == "device") {
    }
    if (print == "pdf") {
        dev.off()
        message("Saved plot of forecast as", filename)
    }
} 