### Time plot: time on x axis, dep var on y axis, groupings are ages
timeplot <- function(d, uncertainty = TRUE, xlab = "Time", ylab = "Data and Forecasts", holdout.times = NULL, 
    insample.obs = TRUE, insample.forecast = FALSE) {
    	
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
    plot.timeprofile <- ggplot(d.forecast, aes_string(x = 'time', y = 'yhat', color = 'age', group = 'age')) + 
        geom_line() + theme_bw() + scale_x_continuous(xlab) + scale_y_continuous(ylab) + scale_color_gradientn("Age", 
        colours = rainbow(7)) + theme(legend.margin = unit(-0.02, "npc"), legend.text = element_text(size = 8))
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
