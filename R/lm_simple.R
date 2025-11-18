#' Fit a Simple Linear Regression Model
#'
#' `lm_simple()` fits a linear regression model using the ordinary least squares
#' (OLS) formula \eqn{(X^T X)^{-1} X^T y}. It is designed as a transparent,
#' minimal implementation similar to base R's `lm()`, but with fewer features.
#'
#' @param formula A formula of the form `y ~ x1 + x2`.
#' @param data A data frame containing the variables used in the formula.
#'
#' @details
#' This function constructs the model matrix using `model.matrix()`, computes
#' OLS estimates directly, and returns fitted values, residuals, and standard
#' errors. It is intended for educational use and demonstration of how OLS
#' works internally.
#'
#' @return
#' A list of class `lm_simple` containing:
#' \itemize{
#'   \item \code{coef} — Estimated regression coefficients
#'   \item \code{se} — Standard errors of the coefficients
#'   \item \code{fitted.values} — Predicted values
#'   \item \code{residuals} — Model residuals
#'   \item \code{vcov} — Variance–covariance matrix of the estimates
#'   \item \code{call} — The matched function call
#' }
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 2*(1:10) + rnorm(10))
#' fit <- lm_simple(y ~ x, data = df)
#' fit$coef
#'
#' @seealso [stats::lm()] for the fully featured base R implementation.
#'
#' @export
lm_simple <- function(formula, data) {
  # Create model matrix
  mf <- model.frame(formula, data)
  Y <- model.response(mf)
  X <- model.matrix(formula, data = mf)

  # Compute OLS estimates
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  rownames(beta) <- colnames(X)

  # Fitted values and residuals
  fitted <- X %*% beta
  residuals <- Y - fitted

  # Standard errors
  sigma2 <- sum(residuals^2) / (nrow(X) - ncol(X))
  vcov <- sigma2 * solve(t(X) %*% X)
  se <- sqrt(diag(vcov))

  # Return object
  res <- list(
    coef = as.vector(beta),
    se = se,
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals),
    vcov = vcov,
    call = match.call()
  )
  class(res) <- "lm_simple"
  res
}

#' Print method for lm_simple
#' @export
print.lm_simple <- function(x, ...) {
  cat("lm_simple model\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coef)
  invisible(x)
}

#' Summary method for lm_simple
#' @export
summary.lm_simple <- function(object, ...) {
  tval <- object$coef / object$se
  pval <- 2 * pnorm(-abs(tval))
  coefmat <- cbind(Estimate = object$coef, "Std. Error" = object$se, "t value" = tval, "Pr(>|t|)" = pval)
  out <- list(call = object$call, coefficients = coefmat)
  class(out) <- "summary.lm_simple"
  out
}

#' Print summary for lm_simple
#' @export
print.summary.lm_simple <- function(x, ...) {
  cat("lm_simple summary\n")
  print(x$call)
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, P.values = TRUE)
  invisible(x)
}
