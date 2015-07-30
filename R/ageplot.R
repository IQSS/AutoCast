### Age plot: age on x axis, dep var on y axis, groupings are times
ageplot <- function(d, uncertainty = FALSE, xlab = "Age", ylab = "Data and Forecasts", holdout.times = NULL, 
    insample.obs = FALSE, insample.forecast = TRUE) {

    sample.frame <- attr(d, "sample.frame")
    if (!is.null(sample.frame)) {
        obs.times <- sample.frame[1]:sample.frame[2]
        forecast.times <- sample.frame[3]:sample.frame[4]
    } else {
        obs.times <- unique(d[!is.na(d$y), "time"])
        times <- unique(d$time)
        forecast.times <- times[!(times %in% obs.times)]
    }
    
    if (!is.null(holdout.times)) {
        d.obs <- d[d$time %in% obs.times & !(d$time %in% holdout.times), ]
        d.holdout <- d[d$time %in% obs.times & d$time %in% holdout.times, ]
        if (insample.forecast) {
            d.forecast <- d
        } else {
            d.forecast <- d[d$time %in% c(holdout.times, forecast.times), ]
        }
    } else {
        d.obs <- d[d$time %in% obs.times, ]
        if (insample.forecast) {
            d.forecast <- d
        } else {
            d.forecast <- d[d$time %in% forecast.times, ]
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
