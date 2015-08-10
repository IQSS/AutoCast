#' Automated Bayesian Forecasting with YourCast
#'
#' Generates Yourcasts under range of prior specifications, ranking forecasts by user preferences for smoothness and out-of-sample fit.
#'
#'
#' @docType package
#' @name AutoCast
#' @import YourCast
#' @import ggplot2
#' @import shiny
#' @importFrom abind abind
#' @importFrom reshape2 melt dcast
NULL

# --------------------- DOCUMENT DATA ---------------------
#' Deaths from breast cancer in the Netherlands
#'
#' A \code{yourprep} object containing breast cancer deaths across 12 age groups in the Netherlands for 1950--2000. 
#' Data contains covariates that are lagged by 30 years. The variables in the data are as follows:
#'
#' \itemize{
#'   \item brst3. Breast cancer deaths.
#'   \item popu3. Population.
#'   \item tobacco2. Tobacco consumption. 
#'   \item gdp. GDP.
#'   \item fat. Fat consumption.
#'   \item hc. Measure of human capital.
#'   \item time. Year covariates measured.
#'   ...
#' }
#'
#' @format A \code{yourprep} object with 12 cross sections (age groups)
#' @source World Health Organization. Available from Federico Girosi and Gary King, 2007, "Cause of Death Data", \url{http://hdl.handle.net/1902.1/UOVMCPSWOL}.
#' @name netherlands_data
#' @references Girosi, Federico, and Gary King. 2008. Demographic Forecasting. Princeton: Princeton University Press. Copy at \url{http://j.mp/pqms4U}.
NULL

#' Selected optimal weights for forecast of breast cancer in the Netherlands
#'
#' A \code{weightcast} object containing a selected weight combination for the forecast of breast cancer in the Netherlands. See \code{\link{weightcast}} for additional details.
#'
#' @format A \code{weightcast} object with a single selected weight combination.
#' @name netherlands_weights
NULL