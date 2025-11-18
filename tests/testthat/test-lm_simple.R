library(testthat)
library(easyLM)

test_that("lm_simple computes correct coefficients", {
  set.seed(123)
  data <- data.frame(x = 1:10, y = 2*(1:10) + rnorm(10))
  fit <- lm_simple(y ~ x, data)
  fit_lm <- lm(y ~ x, data)

  expect_equal(as.numeric(fit$coef), as.numeric(coef(fit_lm)), tolerance = 1e-6)
})

