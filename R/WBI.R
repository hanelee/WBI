#' Wasserstein Bipolarization Index
#'
#' This function takes a vector of observed responses and computes the Wasserstein
#' Bipolarization Index and its asymptotic confidence interval with the maximum
#' polarization distribution (with 0.5 masses each on min and max) as the maximally
#' separated measure.
#'
#' @param obs A vector of observed responses.
#' @param wp The order (p) of the p-Wasserstein distance. Use p=1 to only account for spread, p>1 to account for spread and bi-clustering.
#' @param min The minimum of the response scale.
#' @param max The maximum of the response scale.
#' @param alpha Significance level of the confidence interval.
#' @param wt Weight of each observation, with default set to uniform.
#' @return A vector of length three containing the CI lower bound, point estimate, and CI upper bound.
#' @importFrom dplyr group_by summarize %>%
#' @importFrom transport transport
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' \donttest{
#' # We want to measure WBI of an opinion item measured on [0,100].
#' # We choose W2 distance and 95% asymptotic confidence intervals.
#' data <- c(20, 100, 50, 50, 0,
#'           90, 85, 10, 25, 10,
#'           30, 90, 80, 0, 100,
#'           20, 30, 0, 65, 95)
#' results <- WBI(data, 2, 0, 100, 0.05)
#' }

WBI <- function(obs, wp, min, max, alpha, wt=rep(c(1/length(obs)), length(obs))){

  # Check for stops
  if (any(is.na(obs))) {stop("Observed values contain NAs")}
  if (any(is.na(wt))) {stop("Weights contains NAs")}
  if (length(obs)!=length(wt)) {stop("Length of observed values must match length of weights")}
  if (max <= min) {stop("Max <= min")}
  if (min > min(obs)) {stop("Observed values out of range (min > observed)")}
  if (max < max(obs)) {stop("Observed values out of range (max < observed)")}

  df <- as.data.frame(cbind(obs, wt))
  colnames(df) <- c("obs", "wt")

  # number of observations
  n <-as.integer(length(obs))

  # Aggregate weight per observation value
  obs_sup_freq <- df %>% group_by(obs) %>% summarize(pr = sum(wt))
  obs_sup_freq$pr <- obs_sup_freq$pr/sum(obs_sup_freq$pr) # Normalize
  obs_sup_freq$obs <- (obs_sup_freq$obs - min)/(max-min) # Normalize

  # Support and corresponding mass
  obs_sup <- obs_sup_freq$obs
  obs_freq <- obs_sup_freq$pr

  # Cardinality of support
  k <- length(obs_sup)

  # Transport cost matrix (length(obs_sup), 2)
  cost_matrix <- as.matrix(cbind(obs_sup-rep(0,k), rep(1,k)-obs_sup))**wp

  # Compute transport using transport package
  transportres <- transport(obs_freq, c(0.5, 0.5), costm=cost_matrix, p=1, fullreturn=TRUE)
  dual_opt <- transportres$dual[1:k]
  obj_opt <- transportres$cost**(1/wp)

  # Compute confidence intervals
  sig1 <- (sum(dual_opt**2 * obs_freq) - sum(dual_opt*obs_freq)**2)**(1/2)
  CI_low <- obj_opt - qnorm(1-alpha/2)/(n**(1/2)) * 1/wp * (obj_opt)**(1-wp)*sig1
  CI_high <- obj_opt + qnorm(1-alpha/2)/(n**(1/2)) * 1/wp * (obj_opt)**(1-wp)*sig1

  return(c(CI_low, obj_opt, CI_high))
}
