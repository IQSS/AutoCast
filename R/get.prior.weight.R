get.prior.weight <- function(w, n) {
    if (is.null(w)) {
        return(rep(1, n))
    } else if (length(w) == 1) {
        if (is.na(w) | w == 0) {
            return(rep(1, n))
        } else {
            return(seq(1, n)^w)
        }
    } else if (length(w) == n) {
        return(w)
    } else {
        stop("Weight vector has wrong length.", call. = F)
    }
} 
