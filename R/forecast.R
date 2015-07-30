##### forecast function that runs final forecasts given 'gridcast' or 'groupcast' input as
##### well as the weights from 'weightcast'
##### Author: Konstantin Kashin Date: July 18, 2015

#' @title Run YourCast forecasts given selected weights.
#'
#' @description
#' \code{forecast} runs final YourCast forecasts given a \code{"gridcast"} object and a \code{"weightcast"} object as inputs.
#'
#'
#' @param obj Object of class \code{"gridcast"}. 
#' @param weights Object of class \code{"weightcast"}.
#' @param point.estimate Function applied to calculate the point estimate of the final forecast. Default: \code{median}.
#' @param lower.bound Function applied to calculate the lower bound the final forecast. Default: \code{min}.
#' @param upper.bound Function applied to calculate the upper bound of the final forecast. Default: \code{max}.
#'
#' @return \code{forecast} returns an object of class \code{"forecast"}, which contains the following components:
#' \itemize{
#'      \item \code{yhat} A matrix of forecast point estimates (age groups by time periods).
#'      \item \code{yhat.lower} A matrix of forecast lower bound estimates (age groups by time periods).
#'      \item \code{yhat.upper} A matrix of forecast upper bound estimates (age groups by time periods).
#'      \item \code{y} A matrix of observed data (age groups by time periods).
#'      \item \code{aux} List. A list of inputs to \code{yourcast}.
#'      \item \code{validation} List. A list containing all sigma combinations and the associated diagnostics.
#'      \item \code{weights} The \code{"weightcast"} object provided as an input to \code{forecast}.
#'   }

forecast <- function(obj, weights, point.estimate = median, lower.bound = min, upper.bound = max) {
    if (class(obj) != "gridcast") {
        stop("'obj' must be a 'gridcast' or 'groupcast' object.")
    }
    if (class(weights) != "weightcast") {
        stop("'weights' must a 'weightcast' object.")
    }
    
    # verify that point.estimate, lower.bound, and upper.bound are functions
    
        # extract diags and center / scale, if relevant
        diags <- as.matrix(obj$validation$diags)
        if (!is.null(attr(weights, "diag:center"))) {
            diags <- sweep(diags, MARGIN = 2, attr(weights, "diag:center"))
        }
        if (!is.null(attr(weights, "diag:scale"))) {
            diags <- sweep(diags, MARGIN = 2, attr(weights, "diag:scale"), FUN = "/")
        }
        
        # get dataobj & YourCast args
        args.yourcast <- obj$aux$args.yourcast
        args.yourcast$dataobj <- obj$aux$dataobj
        sigma <- obj$validation$sigma
        
        # run forecasts across weights
        fcastByWeight <- lapply(weights, function(w) {
            obj.fxn <- rowSums(diags * rep(w, each = nrow(diags)))
            opt <- which.min(obj.fxn)
            sigma.opt <- sigma[opt, ]
            args.yourcast.w <- args.yourcast
            args.yourcast.w$Ha.sigma <- as.numeric(sigma.opt[1])
            args.yourcast.w$Ht.sigma <- as.numeric(sigma.opt[2])
            args.yourcast.w$Hat.sigma <- as.numeric(sigma.opt[3])
            fcast <- do.call("yourcast", args.yourcast.w)
            yhat <- t(sapply(fcast$yhat, function(cs) cs[, "yhat"]))
            rownames(yhat) <- obj$aux$ages
            out.w <- list(yhat = yhat, params = fcast$params, opt = as.numeric(opt))
            return(out.w)
        })  # end of lapply over weights
        
        select.runs <- sapply(fcastByWeight, function(el) el$opt)
        select.sigma <- t(sapply(fcastByWeight, function(el) el$params))
        select.yhat <- do.call(abind, list(lapply(fcastByWeight, function(el) el$yhat), along = 3))
        
        # calculate point estimate
        yhat.point <- apply(select.yhat, c(1, 2), point.estimate)
        
        # calculate bounds
        yhat.lower <- apply(select.yhat, c(1, 2), lower.bound)
        yhat.upper <- apply(select.yhat, c(1, 2), upper.bound)
        
        validation <- obj$validation[-which(names(obj$validation) %in% c("yhat.list", "N.runs"))]
        validation$diags <- diags
        validation$selected.runs <- select.runs
        
        out <- list(yhat = yhat.point, yhat.lower = yhat.lower, yhat.upper = yhat.upper, y = obj$y, 
            aux = obj$aux, validation = validation, sigma = select.sigma, weights = weights)
        class(out) <- "forecast"
        return(out)
}  #end of forecast fxn  
