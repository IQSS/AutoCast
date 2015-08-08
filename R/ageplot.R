#' Create a complete age profile ggplot appropriate to a particular data type
#'
#' \code{ageplot} uses ggplot2 to draw a particular plot for an object of a
#' particular class in a single command. This defines the S3 generic that
#' other classes and packages can extend.
#'
#' @param object an object, whose class will determine the behaviour of \code{ageplot}
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' @seealso \code{\link{ggplot}} and \code{\link{fortify}}
ageplot <- function(object, ...) {
  print("calling UseMethod")
  UseMethod("ageplot")
}

#' @export
ageplot.forecast <- function(object, xlab = "Age", ylab = "Data and Forecasts", insample.obs = FALSE, insample.forecast = TRUE, uncertainty = FALSE, ...){
      print("calling ageplot.forecast")
      if (!inherits(object, "forecast"))  stop("use only with \"lm\" objects")
	a <- array(data = NA, dim = c(length(object$aux$ages), length(object$aux$times), 4), dimnames = list(age = object$aux$ages, 
        time = object$aux$times, variable = c("y", "yhat", "yhat.lower", "yhat.upper")))
    a[rownames(object$y), colnames(object$y), "y"] <- object$y
    a[rownames(object$yhat), colnames(object$yhat), "yhat"] <- object$yhat
    a[rownames(object$yhat.lower), colnames(object$yhat.lower), "yhat.lower"] <- object$yhat.lower
    a[rownames(object$yhat.upper), colnames(object$yhat.upper), "yhat.upper"] <- object$yhat.upper
    a <- dcast(melt(a), age + time ~ variable)
    attr(a, "sample.frame") <- object$aux$args.yourcast$sample.frame
	ageplot.default(a, uncertainty=uncertainty, xlab=xlab, ylab=ylab, holdout.times=NULL, insample.obs=insample.obs, insample.forecast=insample.forecast, ...)
}


#variables = list()
#' @export
ageplot.default <- function(object, xlab = "Age", ylab = "Data and Forecasts", holdout.times = NULL, 
    insample.obs = FALSE, insample.forecast = TRUE, uncertainty = FALSE, ...) {
    print("calling ageplot.default")	
    
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
    plot.ageprofile <- ggplot(d.forecast, aes_string(x = 'age', y = 'yhat', color = 'time', group = 'time')) + 
        geom_path() + theme_bw() + scale_x_continuous(xlab) + scale_y_continuous(ylab) + scale_color_gradientn("Time", 
        colours = rainbow(7)) + theme(legend.justification = c(0, 1), legend.position = c(0.05, 
        1), legend.direction = "horizontal", legend.text = element_text(angle = 45), legend.title.align = 1, 
        legend.background = element_rect(fill = "transparent"))
    if (uncertainty) {
        plot.ageprofile <- plot.ageprofile + geom_ribbon(data = d.forecast, aes_string(x = 'age', y = NULL, 
            ymin = 'yhat.lower', ymax = 'yhat.upper', fill = 'time'), alpha = 0.15, color = NA) + 
            scale_fill_gradientn("Time", colours = rainbow(7))
    }
    if (insample.obs) {
        plot.ageprofile <- plot.ageprofile + geom_path(data = d.obs, aes_string(x = 'age', y = 'y', color = 'time', 
            group = 'time'), linetype = "dashed", na.rm = TRUE)
    }
    if (!is.null(holdout.times)) {
        plot.ageprofile <- plot.ageprofile + geom_point(data = d.holdout, aes_string(x = 'age', y = 'y', 
            color = 'time', group = 'time'), shape = 4, na.rm = TRUE)
    }
    
    return(plot.ageprofile)
} 


### create variables = list() option