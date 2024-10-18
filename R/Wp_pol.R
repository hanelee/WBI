#' Calculate the Wasserstein Polarization Distance
#'
#' This function takes a sequence of observed values of an item and a maximally 
#' separated measure with mass q on the minimum (origin) of item and computes the
#' p-Wasserstein distance between the observed measure and the maximally separated
#' measure with (1-alpha)100% confidence intervals.
#'
#'
#' @param obs Vector of observed values from item.
#' @param min Minimum (origin) of item (l).
#' @param max Maximum of item (L).
#' @param q Mass on minimum of item (l) of the maximally separated measure.
#' @param wp p>=1 of p-Wasserstein distance. Default is p=2.
#' @param alpha Alpha of confidence interval. Default is 0.05.
#' @param weights Weights of sample. Default is uniform.
#' @return Vector of length 3: (lower bound of CI, point estimate, upper bound of CI)
#' @export


Wp_pol <- function(obs, min, max, q, wp=2, alpha=0.05, weights=rep(c(1/length(obs)), length(obs))){
  
  # Check for stops
  if (any(is.na(obs))) {stop("Vector of observed values contains NAs")}
  if (any(is.na(weights))) {stop("Vector of weights contains NAs")}
  if (length(obs)!=length(weights)) {stop("Length of observed values must match length of weights")}
  if (max <= min) {stop("Max <= min")}
  if (min > min(obs)) {stop("Observed values out of range (min > observed)")}
  if (max < max(obs)) {stop("Observed values out of range (max < observed)")}
  
  df <- as.data.frame(cbind(obs, weights))
  colnames(df) <- c("obs", "weights")
  
  n <-as.integer(length(obs)) # number of observations
  
  # Aggregate weight per observation value
  obs_sup_freq <- df %>% group_by(obs) %>% summarize(pr = sum(weights))
  obs_sup_freq$pr <- obs_sup_freq$pr/sum(obs_sup_freq$pr) # Normalize 
  obs_sup_freq$obs <- (obs_sup_freq$obs - min)/(max-min) # Normalize
  
  obs_sup <- obs_sup_freq$obs
  obs_freq <- obs_sup_freq$pr
  k <- length(obs_sup)
  
  # Transport package
  cost_matrix <- as.matrix(cbind(obs_sup-rep(0,k), rep(1,k)-obs_sup))**wp
  transportres <- transport(obs_freq, c(q, 1-q), costm=cost_matrix, p=1, fullreturn=TRUE)
  dual_opt <- transportres$dual[1:k]
  obj_opt <- transportres$cost**(1/wp)
  
  sig1 <- (sum(dual_opt**2 * obs_freq) - sum(dual_opt*obs_freq)**2)**(1/2)
  
  CI_low <- obj_opt - qnorm(1-alpha/2)/(n**(1/2)) * 1/wp * (obj_opt)**(1-wp)*sig1
  CI_high <- obj_opt + qnorm(1-alpha/2)/(n**(1/2)) * 1/wp * (obj_opt)**(1-wp)*sig1
  
  return(c(CI_low, obj_opt, CI_high))
}


