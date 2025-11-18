# easyLM

`easyLM` is a simple R package that implements **linear regression** from scratch, designed for teaching and comparison purposes. It provides a lightweight alternative to R’s built-in `lm()` function and demonstrates how linear regression works under the hood.

---

## Features

- Fit a linear regression model with `lm_simple()`
- Compute coefficient estimates, standard errors, and fitted values
- Make predictions on new data with `predict()`
- Compare results with the base `lm()` function for correctness
- Easy-to-read outputs suitable for learning and demonstrations

---

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if you don’t have it
install.packages("devtools")

# Install easyLM from GitHub
devtools::install_github("ntruettner1/easyLM")

Example
library(easyLM)

# Simulate some data
set.seed(123)
x <- rnorm(100)
y <- 2 + 3*x + rnorm(100)
data <- data.frame(x = x, y = y)

# Fit the model
fit <- lm_simple(y ~ x, data = data)

# Show coefficients
print(fit$coef)

# Predict on new data
newdata <- data.frame(x = c(0, 1, 2))
pred <- predict(fit, newdata)
print(pred)

# Compare with base R lm()
fit_lm <- lm(y ~ x, data = data)
all.equal(fit$coef, coef(fit_lm))

Vignette

A full tutorial demonstrating the usage of easyLM can be found in the package vignette:

browseVignettes("easyLM")
