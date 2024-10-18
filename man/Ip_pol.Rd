\name{Ip_pol}
\alias{Ip_pol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Wasserstein Polarization Index
}
\description{
This function takes a sequence of observed values of an item and a maximally separated measure with mass q on the minimum (origin) of item and computes the Wasserstein Polarization Index between the observed measure and the maximally separated measure with (1-alpha)100\% confidence intervals.
}
\usage{
Ip_pol(obs, min, max, q, wp = 2, alpha = 0.05, weights = rep(c(1/length(obs)), length(obs)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{
Vector of observed values from item.
}
  \item{min}{
Minimum (origin) of item (l).
}
  \item{max}{
Maximum of item (L).
}
  \item{q}{
Mass on minimum of item (l) of the maximally separated measure.
}
  \item{wp}{
p>=1 of p-Wasserstein distance. Default is p=2.
}
  \item{alpha}{
Alpha of confidence interval. Default is 0.05.
}
  \item{weights}{
Weights of sample. Default is uniform.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Vector of length 3: (lower bound of CI, point estimate, upper bound of CI)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hane Lee
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (obs, min, max, q, wp = 2, alpha = 0.05, weights = rep(c(1/length(obs)),
    length(obs)))
{
    if (any(is.na(obs))) {
        stop("Vector of observed values contains NAs")
    }
    if (any(is.na(weights))) {
        stop("Vector of weights contains NAs")
    }
    if (length(obs) != length(weights)) {
        stop("Length of observed values must match length of weights")
    }
    if (max <= min) {
        stop("Max <= min")
    }
    if (min > min(obs)) {
        stop("Observed values out of range (min > observed)")
    }
    if (max < max(obs)) {
        stop("Observed values out of range (max < observed)")
    }
    df <- as.data.frame(cbind(obs, weights))
    colnames(df) <- c("obs", "weights")
    n <- as.integer(length(obs))
    obs_sup_freq <- df \%>\% group_by(obs) \%>\% summarize(pr = sum(weights))
    obs_sup_freq$pr <- obs_sup_freq$pr/sum(obs_sup_freq$pr)
    obs_sup_freq$obs <- (obs_sup_freq$obs - min)/(max - min)
    obs_sup <- obs_sup_freq$obs
    obs_freq <- obs_sup_freq$pr
    k <- length(obs_sup)
    cost_matrix <- as.matrix(cbind(obs_sup - rep(0, k), rep(1,
        k) - obs_sup))^wp
    transportres <- transport(obs_freq, c(q, 1 - q), costm = cost_matrix,
        p = 1, fullreturn = TRUE)
    dual_opt <- transportres$dual[1:k]
    obj_opt <- transportres$cost^(1/wp)
    sig1 <- (sum(dual_opt^2 * obs_freq) - sum(dual_opt * obs_freq)^2)^(1/2)
    CI_low <- obj_opt - qnorm(1 - alpha/2)/(n^(1/2)) * 1/wp *
        (obj_opt)^(1 - wp) * sig1
    CI_high <- obj_opt + qnorm(1 - alpha/2)/(n^(1/2)) * 1/wp *
        (obj_opt)^(1 - wp) * sig1
    CI <- c(CI_low, obj_opt, CI_high)
    scale <- max(c(q^(1/wp), (1 - q)^(1/wp)))
    return(1 - rev(CI)/scale)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
