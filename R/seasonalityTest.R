#' Run Seasonality Test
#'
#' This function serves as the main entry point for running seasonality tests on a given time series data.
#' It internally calls `seasonality_test` or `interactive_seasonality_test` based on the parameters passed.
#'
#' @param data The input data for the seasonality test.
#' @param trend The trend type for the seasonality test (default = NULL). Supported options: linear, quadratic, exponential. If NULL, `interactive_seasonality_test` will be called.
#' @param s The seasonality parameter, applicable for the quadratic and exponential trends (default = 12).
#' @param confidence_level The desired confidence level for the statistical tests (default = 0.05).
#' @param summary_data Flag indicating whether to include the results summary (default = TRUE).
#' @return A list containing the result message and the summary statistics for each statistical test.
#' @examples
#' # Example 1: Linear Trend with default Confidence Level and Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "linear", summary_data = TRUE)
#' print(result)
#'
#' # Example 2: Linear Trend without trend parameter
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data)
#' print(result)
#'
#' # Example 2: Quadratic Trend with Different Confidence Level and No Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "quadratic", s = 2, confidence_level = 0.01)
#' print(result)
#'
#' # Example 3: Exponential Trend with Default Confidence Level and Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "exponential", s = 0.5, summary_data = TRUE)
#' print(result)
#'
#' @export
run_seasonality_test <- function(data, trend=NULL, s=12, confidence_level = 0.05, seasons_to_check = 1:s, summary_data = TRUE) {
  if (is.null(trend)) {
    # If trend is NULL, run interactive version of seasonality test
    interactive_seasonality_test(data, s, confidence_level, summary_data)
  } else {
    # If trend is specified, run regular version of seasonality test
    result <- seasonality_test(data, trend, s, confidence_level, summary_data)
    print_result <- result
    print_result$data_ts <- NULL
    print_result$trend_not_specified <- NULL
    print(print_result)
  }
}

seasonality_test <- function(data, trend=NULL, s=12, confidence_level = 0.05, seasons_to_check = 1:s, summary_data = TRUE) {

  #install.packages("crayon")
  require(crayon)
  require(DescTools)

  # Ensure that the data is converted to a time series object
  data <- convert_to_time_series(data, s)

  # Check if confidence_level is between 0 and 1
  if (!is.numeric(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    stop("Invalid confidence_level. It should be a number between 0 and 1.")
  }

  # If the trend was not specified, call the identify_trend function
  trend_not_specified <- is.null(trend)
  if (trend_not_specified) {
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/default.R")
    result <- identify_trend(data, s)

    if (is.null(result$trend)) {
      stop("No clear trend detected in the data.")
    }

    data <- result$data_ts
    trend <- result$trend
  }

  # Check if s belongs to the supported values if not NULL
  if (!is.null(s) &&
      (!is.character(s) && !is.numeric(s)) ||
      !(s %in% c("weekly", "monthly", "quarterly", "yearly", 52, 12, 4, 1))) {
    stop("Invalid value for 's'. Allowed values: 'weekly', 'monthly', 'quarterly', 'yearly' or 52, 12, 4, 1.")
  }


  # Check if trend belongs to the supported values if not NULL
  if (!is.null(trend) && !trend %in% c("linear", "quadratic", "exponential")) {
    stop("Invalid/Unknown trend. Supported options: linear, quadratic, and exponential")
  }

  has_warnings <- FALSE

  if (trend == "linear") {
    # Call the linear function from linear.R
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/linear.R")
    linear_result <- linear(data)
    Ui <- linear_result$Ui
    Vi <- linear_result$Vi
  } else if (trend == "quadratic") {
    # Call the quadratic function from quadratic.R
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/quadratic.R")
    quadratic_result <- quadratic(data, s)
    Ui <- quadratic_result$Ui
    Vi <- quadratic_result$Vi
  } else if (trend == "exponential") {
    # Call the exponential function from exponential.R
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/exponential.R")
    exponential_result <- exponential(data, s)
    Ui <- exponential_result$Ui
    Vi <- exponential_result$Vi
  }  else {
    stop("Invalid trending curve. Supported options: linear, quadratic, exponential")
  }

  is_seasonal <- FALSE

  # Perform the statistical tests if there are enough observations
  # T Test
  t_test_result <- t.test(Ui, Vi, conf.level = (1-confidence_level))
  p_value_t <- t_test_result$p.value

  # SIGN Test
  sign_test_result <- SignTest(Ui, Vi, conf.level = (1-confidence_level))
  p_value_sign <- sign_test_result$p.value

  #Wilcoxon SR test
  wilcox_test_result <- wilcox.test(Ui, Vi, conf.level = (1-confidence_level), paired = TRUE)
  p_value_wilcox <- wilcox_test_result$p.value

  #is_seasonal <- p_value_t < confidence_level || p_value_sign < confidence_level || p_value_wilcox < confidence_level
  is_seasonal <- all(p_value_t < confidence_level, p_value_sign < confidence_level, p_value_wilcox < confidence_level)

  # Calculate relevant statistical summary for each test
  t_summary <- summary(t_test_result)
  sign_summary <- summary(sign_test_result)
  wilcox_summary <- summary(wilcox_test_result)

  # Prepare the results summary
  summary_data <- data.frame(
    Test = c("Student t-Distribution", "Sign Test", "Wilcoxon Signed-Ranks Test"),
    p_value = c(p_value_t, p_value_sign, p_value_wilcox),
    stringsAsFactors = FALSE
  )

  # Prepare the result message
  if(is_seasonal){
    result_message <- paste("There appears to be statistically significant seasonality in the data at the", 100-(confidence_level * 100), "% confidence level.")
  } else {
    result_message <- paste("There does not appear to be statistically significant seasonality in the data at the", 100-(confidence_level * 100), "% confidence level.")
  }

  # Bundle everything into a list to return
  result <- list(
    message = result_message,
    summary_data = summary_data,
    data_ts = if (trend_not_specified) result$data_ts else data,
    trend = trend,
    trend_not_specified = trend_not_specified,
    has_warnings = has_warnings
  )
  return(result)
}

interactive_seasonality_test <- function(data, s=12, confidence_level = 0.05, seasons_to_check = 1:s, summary_data = TRUE) {
  # First, run the seasonality test without specifying a trend
  result <- seasonality_test(data, trend=NULL, s=s, confidence_level=confidence_level, summary_data=summary_data)
  result_to_print <- result
  result_to_print$data_ts <- NULL
  result_to_print$trend <- NULL
  result_to_print$trend_not_specified <- NULL
  print(result_to_print)

  # Then, check if a trend was not specified in the result
  if (result$trend_not_specified) {
    # Print a warning and the identified trend
    cat(red("WARNING - UNRELIABLE RESULT: You did not specify a trend type. For greater accuracy, please specify a trend.\n"))
    cat(blue(paste("Identified trend: ", result$trend, "\n")))

    # Decompose the data and plot the trend component
    decomposed <- decompose(result$data_ts)
    plot(decomposed$trend, main = "Trend Component of Time Series Data", ylab = "Trend")

    # Ask the user if they want to rerun the test with a specified trend
    valid_responses <- c("yes", "Yes", "YES", "No", "NO", "no")
    response <- tolower(readline(prompt = "Would you like to visually inspect the trend and identify the most suitable trend? (yes/no): "))

    while (!response %in% valid_responses) {
      cat(red("Invalid response. Please enter 'yes' or 'no'.\n"))
      response <- tolower(readline(prompt = "Would you like to visually inspect the trend and identify the most suitable trend? (yes/no): "))
    }

    if (tolower(response) == "yes") {
      # Ask the user for the trend to test
      trend <- readline(prompt = "Inspect the plot of the trend and select the most suitable trend (linear, quadratic, exponential): ")

      # Rerun the test with the specified trend
      result <- seasonality_test(data, trend, window, s, confidence_level)

      # Print the result excluding data_ts
      print(result$message)
      print(result$summary_data)
    }
  } else {
    print(result$message)
    print(result$summary_data)
  }
}



run_seasonality_test(sp500__ts)

run_seasonality_test(quadratic_seasonal)
run_seasonality_test(quadratic_seasonal, trend="quadratic", s=12)

sp500__ts
sp500_clean_ts <- ts(as.numeric(sp500__ts), start = start(sp500__ts), frequency = frequency(sp500__ts))
run_seasonality_test(sp500__ts, s="monthly", trend="linear", summary_data = TRUE)
head(sp500__ts, 120)

set.seed(123)  # for reproducibility
t <- 1:120  # time index
seasonality <- 10 * sin(2 * pi * t / 12)  # seasonality component with a 12-month period
trend <- poly(t, 2, raw = TRUE) %*% c(0.5, -0.01)  # quadratic trend component
noise <- rnorm(length(t))  # random noise
seasonal_series <- trend + seasonality + noise
str(seasonal_series)

trend_only_series <- trend + noise
plot(trend_only_series, type = "l")


run_seasonality_test(linear_non_seasonal, s="yearly", trend="linear")
run_seasonality_test(linear_seasonal, s="yearly", trend="linear")

run_seasonality_test(linear_non_seasonal, s="yearly", trend="linear")
run_seasonality_test(seasonal_series, s="monthly", trend="quadratic")

run_seasonality_test(linear_non_seasonal, s="yearly")
run_seasonality_test(linear_seasonal, s="yearly")

# Set seed for reproducibility
set.seed(123)

# Define parameters
n = 120 # number of months for 10 years
slope = 0.05 # slope of the trend
intercept = 2 # y-intercept of the trend

# Generate time index
time_index = 1:n

# Generate linear trend
trend = intercept + slope * time_index

# Generate random noise
noise = rnorm(n, mean = 0, sd = 1) # increase sd to 1

# Combine trend and noise to create the final time series
linear_series = trend + noise

# Plot the series
plot(linear_series, type = "l", main = "Linear Time Series without Seasonality")

# Convert to time-series object
linear_series_ts <- ts(linear_series, frequency = 12)


run_seasonality_test(linear_series_ts, period="quarterly", trend="linear", confidence_level=0.01)
run_seasonality_test(linear_series_ts, period="monthly", trend="linear", confidence_level=0.1)
seasonality_test()
