#' Wasserstein Bipolarization Index with Bootstrap Confidence Intervals
#'
#' This function takes a vector of observed responses and computes the Wasserstein
#' Bipolarization Index and its bootstrap confidence interval with the maximum
#' polarization distribution (with 0.5 masses each on min and max) as the maximally
#' separated measure. We use the package bcaboot for bootstrap.
#'
#' @param obs A vector of observed responses.
#' @param wp The order (p) of the p-Wasserstein distance. Use p=1 to only account for spread, p>1 to account for spread and bi-clustering.
#' @param min The minimum of the response scale.
#' @param max The maximum of the response scale.
#' @param alpha Significance level of the confidence interval.
#' @param r Number of Bootstrap replications
#' @return A vector of length three containing the CI lower bound, point estimate, and CI upper bound.
#' @importFrom bcaboot bcajack
#' @export
#'
#' @examples
#' # We want to measure WBI of an opinion item measured on [0,100].
#' # We choose W2 distance and 95% bootstrap confidence intervals.
#' # r=1 for automatic testing (runs quickly)
#' data <- c(20, 100, 50, 50, 0,
#'           90, 85, 10, 25, 10,
#'           30, 90, 80, 0, 100,
#'           20, 30, 0, 65, 95)
#' results <- WBI_boot(data, 2, 0, 100, 0.05, r=1)
#'

WBI_boot <- function(obs, wp, min, max, alpha, r=5000){

  # Check for stops
  if (any(is.na(obs))) {stop("Observed values contain NAs")}
  if (max <= min) {stop("Max <= min")}
  if (min > min(obs)) {stop("Observed values out of range (min > observed)")}
  if (max < max(obs)) {stop("Observed values out of range (max < observed)")}

  # Estimand function for bcajack
  estimate.wass.bca <- function(data) {
    return(WBI(data, wp, min, max, 0.05)[2])
  }

  # Run BCA
  bca.res <- bcajack(as.matrix(obs), r, estimate.wass.bca, alpha=alpha/2, m=20)

  # Extract confidence limits
  bca.cis <- rev(1-(2**(1/2))*c(bca.res$lims[1,1], bca.res$lims[2,1], bca.res$lims[3,1]))

  return(bca.cis)
}
