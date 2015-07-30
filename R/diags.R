calcDiags <- function(y, aux, validation) {
    pred <- validation$yhat.list
    ages <- as.character(aux$ages)
    times <- as.character(aux$times)
    holdout.years <- as.character(validation$holdout.years)
    time.degree <- validation$time.degree
    
    # RMSE weights
    rmse.time.weights <- get.prior.weight(aux$rmse.age.weight, length(holdout.years))
    rmse.age.weights <- get.prior.weight(aux$rmse.age.weight, length(ages))
    
    # Ha weights
    Ha.age.weights <- get.prior.weight(aux$args.yourcast$Ha.age.weight, length(ages))
    Ha.time.weights <- get.prior.weight(aux$args.yourcast$Ha.time.weight, length(times))
    Ha.age.fd.weights <- (Ha.age.weights[-1] + Ha.age.weights[-length(Ha.age.weights)])/2
    
    # Ht weights
    Ht.age.weights <- get.prior.weight(aux$args.yourcast$Ht.age.weight, length(ages))
    Ht.time.weights <- get.prior.weight(aux$args.yourcast$Ht.time.weight, length(times))
    Ht.time.fd.weights <- (Ht.time.weights[-1] + Ht.time.weights[-length(Ht.time.weights)])/2
    
    # Hat weights
    Hat.age.weights <- get.prior.weight(aux$args.yourcast$Hat.age.weight, length(ages))
    Hat.time.weights <- get.prior.weight(aux$args.yourcast$Hat.time.weight, length(times))
    Hat.age.fd.weights <- (Hat.age.weights[-1] + Hat.age.weights[-length(Hat.age.weights)])/2
    Hat.time.fd.weights <- (Hat.time.weights[-1] + Hat.time.weights[-length(Hat.time.weights)])/2
    
    # RMSE
    rmse <- sqrt(sapply(pred, function(p) {
        e.sq <- (p[, holdout.years] - y[, holdout.years])^2
        e.sq.time <- apply(e.sq, MARGIN = 1, function(x) weighted.mean(x, rmse.time.weights))
        return(weighted.mean(e.sq.time, rmse.age.weights))
    }))
    
    ### AGE
    
    # average observed age profile
    y.mean <- rowMeans(y)
    # now get smoothed observed age profile
    spline.out <- fitted(smooth.spline(x = as.numeric(ages), y = y.mean))
    smoothed.age <- data.frame(age = as.numeric(ages), smoothed.y = spline.out, mean = y.mean)
    
    age.arc <- sapply(pred, function(p) {
        pred.age.fd <- apply(p, MARGIN = 2, function(x) abs(diff(x - spline.out)))
        return(weighted.mean(apply(pred.age.fd, MARGIN = 1, function(x) weighted.mean(x, Ha.time.weights)), 
            Ha.age.fd.weights))
    })
    
    ### TIME
    formula <- as.formula("y ~ 1")
    # Add terms up to desired degree
    if (time.degree > 0) {
        terms <- 1:time.degree
        x.terms <- paste("+ I(time^", terms, ")", sep = "", collapse = "")
        formula <- update.formula(formula, paste("~ .", x.terms, sep = ""))
    }
    
    time.arc <- sapply(pred, function(p) {
        pred.time.fd <- t(apply(p, 1, function(y) {
            dat <- data.frame(y = y, time = as.numeric(times))
            return(abs(diff(resid(lm(formula, dat)))))
        }))
        return(weighted.mean(apply(pred.time.fd, MARGIN = 1, function(x) weighted.mean(x, 
            Ht.time.fd.weights)), Ht.age.weights))
    })
    
    ### TIME TREND
    time.trend.arc <- sapply(pred, function(p) {
        pred.dt <- p[, -1] - p[, -ncol(p)]
        pred.da <- pred.dt[-1, ] - pred.dt[-nrow(pred.dt), ]
        return(weighted.mean(apply(abs(pred.da), MARGIN = 1, function(x) weighted.mean(x, 
            Hat.time.fd.weights)), Hat.age.fd.weights))
    })
    
    diags <- data.frame(rmse = rmse, age.arc = age.arc, time.arc = time.arc, time.trend.arc = time.trend.arc)
    
    return(diags)
} 
