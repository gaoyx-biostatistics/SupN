#' @title Logrank Tests (Freedman)
#' @description  Two Survival Curves,Test (Inequality)
#'
#' @param alpha type I error rate
#' @param beta 1-power
#' @param k Group Allocation experimental group/control group
#' @param pl Proportion Lost During Follow Up
#' @param HRtype Survival Proportions
#' @param param1 control group
#' @param param2 experimental group
#'
#' @return N
#' @export
#' @import stats
#'
#' @examples
#' logrank.freedman(
#'   alpha = 0.05, beta = 1 - 0.70137, k = 1, pl = 0,
#'   HRtype = "survival.proportions", param1 = 0.5, param2 = 0.6
#' )
logrank.freedman <- function(alpha, beta, k, pl, HRtype = c("hazard.rates", "survival.proportions", "median.survival.times"), param1, param2) {
  HRtype <- match.arg(HRtype)
  HRtype <- switch(HRtype,
    hazard.rates = 1,
    survival.proportions = 2,
    median.survival.times = 3
  )

  if (HRtype == 1) {
    HR <- param2 / param1
  } else if (HRtype == 2) {
    HR <- log(param2) / log(param1)
  } else {
    HR <- param1 / param2
  }
  w <- pl
  body <- quote({
    abs(HR - 1) * sqrt(N * (1 - w) * k * ((1 - param1) + k * (1 - param2)) / (1 + k)) / (1 + k * HR) - qnorm(1 - alpha / 2) - qnorm(1 - beta)
  })

  N <- uniroot(function(N) eval(body), c(2 + 1e-10, 1e+09))$root
  N
}
