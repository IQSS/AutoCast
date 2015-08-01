#' Create a complete time profile ggplot appropriate to a particular data type
#'
#' \code{timeplot} uses ggplot2 to draw a particular plot for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of \code{timeplot}
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' @seealso \code{\link{ggplot}} and \code{\link{fortify}}
timeplot <- function(object, ...) {
  print("calling UseMethod")
  UseMethod("timeplot")
}

#' @export
timeplot.forecast <- function(object, xlab="Age", ylab = "Data and Forecasts", insample.obs = FALSE, insample.forecast = TRUE, 
        uncertainty = FALSE, ...){
      print("calling timeplot.forecast")
      if (!inherits(object, "forecast"))  stop("use only with \"lm\" objects")
	a <- array(data = NA, dim = c(length(object$aux$ages), length(object$aux$times), 4), dimnames = list(age = object$aux$ages, 
        time = object$aux$times, variable = c("y", "yhat", "yhat.lower", "yhat.upper")))
    a[rownames(object$y), colnames(object$y), "y"] <- object$y
    a[rownames(object$yhat), colnames(object$yhat), "yhat"] <- object$yhat
    a[rownames(object$yhat.lower), colnames(object$yhat.lower), "yhat.lower"] <- object$yhat.lower
    a[rownames(object$yhat.upper), colnames(object$yhat.upper), "yhat.upper"] <- object$yhat.upper
    a <- dcast(melt(a), age + time ~ variable)
    attr(a, "sample.frame") <- object$aux$args.yourcast$sample.frame
	timeplot.default(a, uncertainty=uncertainty, xlab=xlab, ylab=ylab, holdout.times=NULL, insample.obs=insample.obs, insample.forecast=insample.forecast, ...)
}


#' @export
timeplot.default <- function(object, uncertainty = FALSE, xlab = "Time", ylab = "Data and Forecasts", holdout.times = NULL, 
    insample.obs = TRUE, insample.forecast = FALSE, ...) {
    print("calling timeplot.default")	
    
    if(!is.data.frame(object)){
    	stop("object must be data.frame with the following 4 columns: time, age, y, yhat", call.=F)
    }
    if(!all(c("time","age","y","yhat") %in% colnames(object))){
    	stop("object must be data.frame with the following 4 columns: time, age, y, yhat", call.=F)
    }
    
    sample.frame <- attr(object, "sample.frame")
    if (!is.null(sample.frame)) {
        obs.times <- sample.frame[1]:sample.frame[2]
        forecast.times <- sample.frame[3]:sample.frame[4]
    } else {
        obs.times <- unique(object[!is.na(object$y), "time"])
        times <- unique(object$time)
        forecast.times <- times[!(times %in% obs.times)]
    }
    
    if (!is.null(holdout.times)) {
        d.obs <- object[object$time %in% obs.times & !(object$time %in% holdout.times), ]
        d.holdout <- object[object$time %in% obs.times & object$time %in% holdout.times, ]
        if (insample.forecast) {
            d.forecast <- object
        } else {
            d.forecast <- object[object$time %in% c(holdout.times, forecast.times), ]
        }
    } else {
        d.obs <- object[object$time %in% obs.times, ]
        if (insample.forecast) {
            d.forecast <- object
        } else {
            d.forecast <- object[object$time %in% forecast.times, ]
        }
    }
    
    # plot
    plot.timeprofile <- ggplot(d.forecast, aes_string(x = 'time', y = 'yhat', color = 'age', group = 'age')) + 
        geom_line() + theme_bw() + scale_x_continuous(xlab) + scale_y_continuous(ylab) + scale_color_gradientn("Age", 
        colours = rainbow(7)) + theme(legend.margin = grid::unit(-0.02, "npc"), legend.text = element_text(size = 8))
    if (uncertainty) {
        plot.timeprofile <- plot.timeprofile + geom_ribbon(data = d.forecast, aes_string(x = 'time', 
            y = NULL, ymin = 'yhat.lower', ymax = 'yhat.upper', fill = 'age'), alpha = 0.15, color = NA) + 
            scale_fill_gradientn("Age", colours = rainbow(7))
    }
    if (insample.obs) {
        plot.timeprofile <- plot.timeprofile + geom_path(data = d.obs, aes_string(x = 'time', y = 'y', 
            color = 'age', group = 'age'), linetype = "dashed", na.rm = TRUE)
    }
    if (!is.null(holdout.times)) {
        plot.timeprofile <- plot.timeprofile + geom_point(data = d.holdout, aes_string(x = 'time', 
            y = 'y', color = 'age', group = 'age'), shape = 4, na.rm = TRUE)
    }
    return(plot.timeprofile)
} 
