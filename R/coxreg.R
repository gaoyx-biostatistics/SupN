#' Cox Regression
#'
#' @param alpha type I error rate
#' @param beta 1-power
#' @param P  the proportion of subjects that fail
#' @param B Log Hazard Ratio
#' @param RSquared This is the R-Squared that is obtained when X1
#'  is regressed on the other Xs (covariates) in the model
#' @param sigma Standard Deviation of X1
#'
#' @return N
#' @export
#'
#' @examples
#' coxreg(0.05, 0.2, 0.738, 1, 0.1837, 0.3126)
coxreg <- function(alpha, beta, P, B, RSquared, sigma) {
  D <- quote({
    (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 / ((1 - RSquared) * sigma^2 * B^2)
  })
  N <- uniroot(function(N) eval(D) / P - N, c(2 + 1e-10, 1e+09))$root
  return(N)
}
