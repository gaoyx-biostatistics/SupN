#' @title Title Tests for One Proportion
#' @description Z Test using S(P0)
#' @param alternative Two-Sided or One-side
#' @param beta  1-power
#' @param alpha  type I error rate
#' @param p0  Null Proportion
#' @param p1 Alternative Proportion
#'
#' @return n
#' @export
#'
#' @examples
#' TestsforOneProportion(alternative = "two", beta = 0.1, alpha = 0.05, p0 = 0.5, p1 = 0.55)
#' TestsforOneProportion(alternative = "upper", beta = 0.1, alpha = 0.05, p0 = 0.5, p1 = 0.55)
#' TestsforOneProportion(alternative = "lower", beta = 0.1, alpha = 0.05, p0 = 0.5, p1 = 0.45)

TestsforOneProportion <- function(alternative = c("two", "upper", "lower"), beta, alpha, p0, p1) {
  alternative <- switch(alternative,
    two = 2,
    upper = 1,
    lower = 0
  )
  deno <- sqrt(p1 * (1 - p1))

  if (alternative == 2) {
    body <- quote(
      function(n) {
        pnorm(sqrt(n) * (p0 - p1) / deno - qnorm(1 - alpha / 2) * sqrt(p0 * (1 - p0)) / deno) - pnorm(
          sqrt(n) * (p0 - p1) / deno + qnorm(1 - alpha / 2) * sqrt(p0 * (1 - p0)) / deno
        ) + beta
      }
    )
    n <- uniroot(eval(body), c(2 + 1e-10, 1e+09))$root
    return(n)
  }

  if (alternative == 1) {
    body <- quote(
      function(n) {
        pnorm(sqrt(n) * (p0 - p1) / deno + qnorm(1 - alpha) * sqrt(p0 * (1 - p0)) / deno) - beta
      }
    )
    n <- uniroot(eval(body), c(2 + 1e-10, 1e+09))$root
    return(n)
  }

  if (alternative == 0) {
    body <- quote(
      function(n) {
        pnorm(sqrt(n) * (p0 - p1) / deno - qnorm(1 - alpha) * sqrt(p0 * (1 - p0)) / deno) - beta
      }
    )
    n <- uniroot(eval(body), c(2 + 1e-100, 1e+100))$root
    return(n)
  }
}

