OneSampleTtest_inequality <- function(alpha,delta,alternative= c("two.side","upper.tail","lower.tail"),
                                      )
alternative <- switch (alternative,
  two.side = 0,
  upper.tail=1,
  lower.tail=2
)
