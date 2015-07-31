##### gridcast function that sets up multiple runs of YourCast across a grid of prior
##### parameters (sigma grid) outputs the YourCast results as well as diagnostics across runs
##### Author: Konstantin Kashin Date: July 18, 2015

#' @title Automated Bayesian forecasting with YourCast
#'
#' @description
#' \code{gridcast} generates YourCast forecasts for a range of prior specifications and outputs diagnostics across the different specifications.
#'
#'
#' @param dataobj Object of class '\code{yourprep}'
#' @param Ha.sigma.seq Three element numeric vector. The first two elements give the range of \code{Ha.sigma} values to search over. 
#'   The 3rd element gives the number of values to sample from the range. 
#'   If do not want to use age smoothing, set as \code{NA} and leave \code{Ha.list} as \code{NULL}.
#' @param Ha.list Vector. A vector of additional values of \code{Ha.sigma} to include in the grid search.Default: \code{NULL}.
#' @param Ht.sigma.seq Three element numeric vector. The first two elements give the range of \code{Ht.sigma} values to search over.
#'   The 3rd element gives the number of values to sample from the range. 
#'   If do not want to use time smoothing, set as \code{NA} and leave \code{Ht.list} as \code{NULL}.
#' @param Ht.list Vector. A vector of additional values of \code{Ht.sigma} to include in the grid search. Default: \code{NULL}.
#' @param Hat.sigma.seq Three element numeric vector. The first two elements give the range of \code{Hat.sigma} values to search over. 
#'    The 3rd element gives the number of values to sample from the range.
#'   If do not want to use trend smoothing, set as \code{NA} and leave \code{Hat.list} as \code{NULL}.
#' @param Hat.list Vector. A vector of additional values of \code{Hat.sigma} to include in the grid search. Default: \code{NULL}.
#' @param logscale Logical. Should sigma values be evenly sampled on a log scale and then exponentiated? Default: \code{TRUE}.
#' @param time.degree Non-negative integer. Specifies the degree of the baseline polynomial to which time profiles are smoothed. 
#'   For example, if \code{time.degree=1}, then the forecasts cllosest to a straight line are scored highest. 
#'   If \code{time.degree}=0, then forecasts closest to a flat line are scored highest.
#' @param length.holdout Non-negative integer. Specifies the number of time periods that should be omitted from the end of the observed period for validation. 
#'   If set to 0, then at least one time period must be provided as an argument to \code{holdout}.
#' @param holdout Vector specifying additional time periods to withhold for validation. Default: \code{NULL}. 
#' @param mse.age.weight A scalar or a numeric vector with weights that determine how much the different age groups factor into the calculation of root mean square error of the forecasts in the validation period. 
#'   If set to \code{0} or \code{NA}, age groups are weighted equally; 
#'   if set to a nonzero scalar, the weight for age group a is set proportional to a^\code{mse.age.weight}; 
#'   if a vector of length A, the ath element is the weight of age group a. Default: \code{0}. 
#' @param print.runs Logical. If TRUE, will print notification for each run of yourcast.
#' @param ... Additional arguments passed to \code{yourcast} function.

#' @return An object of class \code{"gridcast"} contains the following components
#' \itemize{
#'      \item \code{y} A matrix of observed data (age groups by time periods).
#'      \item \code{aux} List. A list of inputs to \code{yourcast}.
#'      \item \code{validation} List. A list containing all sigma combinations, the forecasts, and the associated diagnostics.
#'   }


#' @export
gridcast <- function(dataobj, Ha.sigma.seq = c(from = 0.01, to = 20, length.out = 5), Ha.list = NULL, 
    Ht.sigma.seq = c(from = 0.01, to = 20, length.out = 5), Ht.list = NULL, Hat.sigma.seq = NA, 
    Hat.list = NULL, logscale = TRUE, time.degree = 1, length.holdout = 5, holdout = NULL, 
    mse.age.weight = NA, print.runs = TRUE, ...) {
    
    ##### GET YOURCAST DATA OBJECT #####
    if (is.null(dataobj) | (class(dataobj) != "yourprep")) {
        stop("A dataobj argument of class 'yourprep' is required. See help(yourcast) for more details.", 
            call. = F)
    }
    
    # Check to see only one geographical area in data object Parse index.code to find out how
    # to extract geo codes
    index.code <- dataobj$index.code
    split.index <- strsplit(index.code, "")[[1]]
    N.g <- length(split.index[split.index == "g"])
    csids <- names(dataobj$data)  # names on cross-sections
    geo.codes <- sapply(csids, substr, start = 1, stop = N.g)  # extract geo codes from names on cross-sections
    if (length(unique(geo.codes)) > 1) {
        stop("Function currently only capable to processing single geographic unit at a time. Please load only one CSID code into dataobj.", 
            call. = F)
    }
    
    ##### PARSE YOURCAST ARGUMENTS ##### See if necessary yourcast inputs supplied; if not grab
    ##### defaults from yourcast fxn If necessary inputs supplied, need to move them out of '...'
    ##### if(is.null(auto)){ # comment out for now, not sure we need feature that allows user to
    ##### load auto
    args.yourcast <- list(...)
    yc.in <- names(args.yourcast)
    if (length(grep("sample.frame", yc.in)) == 0) {
        sample.frame <- formals(yourcast)$sample.frame
    } else {
        sample.frame <- args.yourcast$sample.frame
    }
    if (length(grep("formula", yc.in)) == 0) {
        formula <- formals(yourcast)$formula
    } else {
        formula <- args.yourcast$formula <- as.formula(args.yourcast$formula)
        environment(args.yourcast$formula) <- NULL
    }
    if (length(grep("model", yc.in)) == 0) {
        model <- formals(yourcast)$model
    } else {
        model <- args.yourcast$model
    }
    if (model == "OLS") {
        Ha.sigma.seq <- NA
        Ht.sigma.seq <- NA
        Hat.sigma.seq <- NA
    }
    
    
    # set up auxiliary lholder
    aux <- list(args.yourcast = args.yourcast, mse.age.weight = mse.age.weight)
    
    ##### PARSE MODEL DETAILS #####
    
    # What are the age groups?
    N.a <- length(split.index[split.index == "a"])
    aux$ages <- as.numeric(sapply(csids, substr, start = N.g + 1, stop = N.g + N.a))
    age.min <- min(aux$ages)
    age.max <- max(aux$ages)
    
    # What are the in and out sample years? (parse from sample.frame)
    year.min <- sample.frame[1]
    year.max <- sample.frame[2]
    year.maxf <- sample.frame[4]
    aux$times <- year.min:year.maxf
    aux$obs.times <- year.min:year.max
    
    # What is the response variable?
    response <- deparse(attr(terms(formula), which = "variables")[[2]])
    
    # Store complete observed data as age x time matrix NOTE: this is the outcome in the
    # transformed form (if applicable)
    y <- t(sapply(dataobj$data, function(x) {
        x <- as.data.frame(x)[as.character(year.min:year.max), ]
        resp <- eval(parse(text = response), envir = x)
        return(resp)
    }))
    colnames(y) <- aux$obs.times
    rownames(y) <- aux$ages
    
    ##### SETUP SIGMA GRID #####
    
    # if logscale=TRUE, setup equal intervals on log scale (else on regular scale)
    getseq <- function(s1, s2, n) {
        ifelse(is.na(n), return(NA), ifelse(logscale, return(logseq(s1, s2, length.out = n)), 
            return(seq(s1, s2, length.out = n))))
    }
    
    sigma.input <- list(Ha.sigma = Ha.sigma.seq, Ht.sigma = Ht.sigma.seq, Hat.sigma = Hat.sigma.seq)
    sigma.list <- lapply(sigma.input, function(s) getseq(s[1], s[2], s[3]))
    sigma <- do.call(expand.grid, sigma.list)
    
    # Need more than one sigma combination
    if (nrow(sigma) == 1) {
        stop("Need more than one sigma combination...please increase N.Ha, N.Ht, or N.Hat", 
            call. = F)
    }
    
    
    ##### SET UP HOLDOUT SAMPLE FOR VALIDATION #####
    
    # set up candidate years to omit
    candidate.years.omit <- seq(year.min + 1, year.max)
    
    # if no holdout times are specified, use last years
    if (is.null(holdout)) {
        if (length(candidate.years.omit) < length.holdout) {
            stop("You have specified a holdout period longer than the observed period (not including first period).", 
                call. = F)
        }
        holdout.years <- candidate.years.omit[(length(candidate.years.omit) - length.holdout + 
            1):length(candidate.years.omit)]
    } else {
        if (all(holdout %in% candidate.years.omit)) {
            holdout.years <- as.integer(holdout)
        } else {
            stop("You have specified a holdout time that is either the first period (not allowed) or a time not in the observed period.", 
                call. = F)
        }
    }
    
    # Create heldout dataobj
    dataobjValid <- createValidObj(holdout.years = holdout.years, dataobj = dataobj, response = all.vars(formula)[1])
    
    ##### RUN YOURCAST ##### Run yourcast with each of the combinations of priors Uses
    ##### 'parse.yourcast' to load prior values into yourcast properly
    N.runs <- nrow(sigma)
    
    if (print.runs) {
        cat(paste("Starting", N.runs, "yourcast() runs\n"))
    }
    
    yourcastRuns <- lapply(X = 1:N.runs, FUN = function(i) {
        sigmaRun <- as.numeric(sigma[i, ])
        yourcastRun <- do.call("parse_auto", append(list(prior.vec = sigmaRun, run = i, verb.parse = print.runs, 
            dataobj = dataobjValid), args.yourcast))
        return(yourcastRun)
    })
    
    # create a holder for validation output
    validation <- list(sigma = sigma, N.runs = N.runs, holdout.years = holdout.years, time.degree = time.degree)
    
    # create a list of forecast matrices (age x time matrix)
    validation$yhat.list <- lapply(yourcastRuns, function(r) {
        out <- t(sapply(r$yhat, function(cs) cs[, "yhat"]))
        rownames(out) <- aux$ages
        return(out)
    })
    
    names(validation$yhat.list) <- apply(sigma, 1, function(x) paste(round(x, 2), collapse = "-"))
    
    ##### CALCULATE DIAGNOSTICS #####
    validation$diags <- calcDiags(y = y, aux = aux, validation = validation)
    
    ##### FINALIZE OUTPUTS #####
    aux$dataobj <- dataobj
    out <- list(y = y, aux = aux, validation = validation)
    class(out) <- "gridcast"
    return(out)
}  # end of gridcast 
