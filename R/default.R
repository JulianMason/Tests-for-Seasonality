# Load necessary packages for testing
library(testthat)
library(seasonalityTest)
library(stats)

# Context for testing seasonalityTest package
context("Testing seasonalityTest package")

# Helper function to test for errors
expect_error_message <- function(expr, expected_message) {
  expect_error(expr, regexp = expected_message, fixed = TRUE)
}

# Test case 1: Basic functionality with linear trend
test_that("Test seasonality_test with linear trend", {
  data <- c(10, 15, 20, 15, 10)

  result <- seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
  expect_equal(result$message, "Seasonality detected with linear trend.")
  expect_true(all(result$summary$coefficients[, 4] < 0.05))  # Assuming significance level is 0.05
})

# Test case 2: Quadratic trend with custom parameters
test_that("Test seasonality_test with quadratic trend and custom parameters", {
  data <- c(10, 20, 30, 20, 10)

  result <- seasonality_test(data, trend = "quadratic", s = 4, confidence_level = 0.1)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
  expect_equal(result$message, "Seasonality detected with quadratic trend.")
  expect_true(all(result$summary$coefficients[, 4] < 0.1))  # Custom confidence level
})

# Test case 3: No seasonality detected
test_that("Test seasonality_test with no seasonality", {
  data <- c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)  # Flat line, no seasonality

  result <- seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
  expect_equal(result$message, "No significant seasonality detected.")
  expect_true(all(result$summary$coefficients[, 4] >= 0.05))  # Assuming significance level is 0.05
})

# Test case 4: Empty input data
test_that("Test seasonality_test with empty input", {
  data <- numeric(0)

  expect_error_message(seasonality_test(data, trend = "linear"), "Data should be numeric and non-empty.")
})

# Test case 5: Non-numeric input data
test_that("Test seasonality_test with non-numeric input", {
  data <- c("a", "b", "c")

  expect_error_message(seasonality_test(data, trend = "linear"), "Data should be numeric and non-empty.")
})

# Test case 6: High frequency data with valid seasonality
test_that("Test seasonality_test with high frequency data", {
  data <- c(10, 20, 30, 20, 10, 30, 40, 50, 40, 30, 20, 10)  # Seasonal pattern

  result <- seasonality_test(data, trend = "linear", s = 12)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
  expect_equal(result$message, "Seasonality detected with linear trend.")
  expect_true(all(result$summary$coefficients[, 4] < 0.05))  # Assuming significance level is 0.05
})

# Test case 7: Testing summary data option
test_that("Test seasonality_test with summary_data option", {
  data <- c(10, 20, 30, 20, 10)

  result <- seasonality_test(data, trend = "linear", summary_data = FALSE)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "data"))
  expect_equal(result$message, "Seasonality detected with linear trend.")
})

# Test case 8: Testing different trend types
test_that("Test seasonality_test with different trend types", {
  data <- c(10, 15, 20, 15, 10)

  result_linear <- seasonality_test(data, trend = "linear")
  result_quadratic <- seasonality_test(data, trend = "quadratic")
  result_exponential <- seasonality_test(data, trend = "exponential")

  expect_true(all(result_linear$summary$coefficients[, 4] < 0.05))
  expect_true(all(result_quadratic$summary$coefficients[, 4] < 0.05))
  expect_true(all(result_exponential$summary$coefficients[, 4] < 0.05))
})

# Test case 9: Test with large dataset
test_that("Test seasonality_test with large dataset", {
  data <- rnorm(1000)  # Random normal data

  result <- seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
})

# Test case 10: Test with missing values
test_that("Test seasonality_test with missing values", {
  data <- c(10, 20, NA, 30, 40)

  expect_error_message(seasonality_test(data, trend = "linear"), "Data cannot contain NA or Inf values.")
})

# Test case 11: Test with outliers
test_that("Test seasonality_test with outliers", {
  data <- c(10, 15, 20, 1000, 10, 15, 20)

  result <- seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary", "data"))
  expect_equal(result$message, "Seasonality detected with linear trend.")
})

# Test case 12: Test with short time series
test_that("Test seasonality_test with short time series", {
  data <- c(10, 15)

  expect_error_message(seasonality_test(data, trend = "linear"), "Insufficient amount of data. Need sufficient data for at least 2 complete chosen seasons")
})

