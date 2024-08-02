# Load necessary packages for testing
library(testthat)
library(seasonalityTest)
library(stats)
library(DescTools)

# Load the internal dataset
data(internal_dataset)

# Helper function to test for errors
expect_error_message <- function(expr, expected_message) {
  expect_error(expr, regexp = expected_message, fixed = TRUE)
}

# Test case 1: Basic functionality with linear trend
test_that("Test seasonality_test with linear trend", {
  data <- internal_dataset$linear_seasonal
  confidence_level <- 0.05
  result <- run_seasonality_test(data, trend = "linear", confidence_level = confidence_level)
  confidence_percent <- 100 * (1 - confidence_level)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
  expect_equal(result$message, paste("There appears to be statistically significant seasonality in the data at the", confidence_percent, "% confidence level."))
  expect_equal(result$trend, "linear")
  expect_false(result$has_warnings)
})

# Test case 2: Quadratic trend with custom parameters
test_that("Test seasonality_test with quadratic trend and custom parameters", {
  data <- internal_dataset$quadratic_seasonal
  confidence_level <- 0.1
  result <- run_seasonality_test(data, trend = "quadratic", s = 4, confidence_level = confidence_level)
  confidence_percent <- 100 * (1 - confidence_level)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
  expect_equal(result$message, paste("There appears to be statistically significant seasonality in the data at the", confidence_percent, "% confidence level."))
  expect_equal(result$trend, "quadratic")
  expect_false(result$has_warnings)
})

# Test case 3: No seasonality detected
test_that("Test seasonality_test with no seasonality", {
  data <- internal_dataset$linear_non_seasonal
  confidence_level <- 0.05
  result <- run_seasonality_test(data, trend = "linear", confidence_level = confidence_level)
  confidence_percent <- 100 * (1 - confidence_level)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
  expect_equal(result$message, paste("There does not appear to be statistically significant seasonality in the data at the", confidence_percent, "% confidence level."))
  expect_equal(result$trend, "linear")
  expect_false(result$has_warnings)
})

# Test case 4: Empty input data
test_that("Test seasonality_test with empty input", {
  data <- numeric(0)
  expect_error_message(run_seasonality_test(data, trend = "linear"), "Data should be numeric and non-empty.")
})

# Test case 5: Non-numeric input data
test_that("Test seasonality_test with non-numeric input", {
  data <- internal_dataset$non_numeric
  expect_error_message(run_seasonality_test(data, trend = "linear"), "Data should be numeric and non-empty.")
})

# Test case 6: High frequency data with valid seasonality
test_that("Test seasonality_test with high frequency data", {
  data <- internal_dataset$linear_seasonal
  confidence_level <- 0.05
  result <- run_seasonality_test(data, trend = "linear", s = 12, confidence_level = confidence_level)
  confidence_percent <- 100 * (1 - confidence_level)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
  expect_equal(result$message, paste("There appears to be statistically significant seasonality in the data at the", confidence_percent, "% confidence level."))
  expect_equal(result$trend, "linear")
  expect_false(result$has_warnings)
})

# Test case 7: Testing summary data option
test_that("Test seasonality_test with summary_data option", {
  data <- internal_dataset$linear_seasonal
  result <- run_seasonality_test(data, trend = "linear", summary_data = FALSE)

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "trend", "has_warnings"))
  expect_equal(result$message, "There appears to be statistically significant seasonality in the data at the 95 % confidence level.")
})

# Test case 8: Testing different trend types
test_that("Test seasonality_test with different trend types", {
  data <- internal_dataset$linear_seasonal
  result_linear <- run_seasonality_test(data, trend = "linear")
  result_quadratic <- run_seasonality_test(data, trend = "quadratic")
  result_exponential <- run_seasonality_test(data, trend = "exponential")

  expect_true(all(result_linear$summary_df$p_value < 0.05))
  expect_true(all(result_quadratic$summary_df$p_value < 0.05))
  expect_true(all(result_exponential$summary_df$p_value < 0.05))
})

# Test case 9: Test with large dataset
test_that("Test seasonality_test with large dataset", {
  data <- internal_dataset$generic_data
  result <- run_seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
})

# Test case 10: Test with missing values
test_that("Test seasonality_test with missing values", {
  data <- internal_dataset$anomalous_data
  expect_error_message(run_seasonality_test(data, trend = "linear"), "Data cannot contain NA or Inf values.")
})

# Test case 11: Test with outliers
test_that("Test seasonality_test with outliers", {
  data <- internal_dataset$linear_seasonal
  data[5] <- 1000  # Introduce an outlier
  result <- run_seasonality_test(data, trend = "linear")

  expect_true(is.list(result))
  expect_equal(names(result), c("message", "summary_df", "trend", "has_warnings"))
  expect_equal(result$message, "There appears to be statistically significant seasonality in the data at the 95 % confidence level.")
})

# Test case 12: Test with short time series
test_that("Test seasonality_test with short time series", {
  data <- internal_dataset$linear_seasonal[1:5]
  expect_error_message(run_seasonality_test(data, trend = "linear"), "Insufficient amount of data. Need sufficient data for at least 2 complete chosen seasons. The default 's' is 12 (monthly), so the data should have at least 24 observations. Allowed values for 's' are 'weekly' (52), 'monthly' (12), 'quarterly' (4), 'yearly' (1).")
})

# Additional test cases for default.R
test_that("convert_to_time_series handles different frequency values", {
  data <- c(1:104)  # Sufficient data for weekly frequency
  ts_data_weekly <- convert_to_time_series(data, s = "weekly")
  expect_equal(frequency(ts_data_weekly), 52)

  data <- c(1:24)  # Sufficient data for monthly frequency
  ts_data_monthly <- convert_to_time_series(data, s = "monthly")
  expect_equal(frequency(ts_data_monthly), 12)

  data <- c(1:8)  # Sufficient data for quarterly frequency
  ts_data_quarterly <- convert_to_time_series(data, s = "quarterly")
  expect_equal(frequency(ts_data_quarterly), 4)

  data <- c(1:2)  # Sufficient data for yearly frequency
  ts_data_yearly <- convert_to_time_series(data, s = "yearly")
  expect_equal(frequency(ts_data_yearly), 1)
})

test_that("convert_to_time_series handles invalid frequency values", {
  data <- c(1:24)
  expect_error(convert_to_time_series(data, s = "invalid"), "Invalid value for 's'.")
  expect_error(convert_to_time_series(data, s = 99), "Invalid value for 's'. Allowed values: 'weekly', 'monthly', 'quarterly', 'yearly' or 52, 12, 4, 1.")
})
