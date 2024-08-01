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
#' data <- c(1:48)
#' result <- run_seasonality_test(data, trend = "linear", summary_data = TRUE)
#' print(result)
#'
#' # Example 2: Linear Trend without specifying trend parameter
#' data <- c(1:48)
#' result <- run_seasonality_test(data)
#' print(result)
#'
#' # Example 3: Quadratic Trend with Different Confidence Level and No Summary Data
#' data <- c(1:48)
#' result <- run_seasonality_test(data, trend = "quadratic", s = 2, confidence_level = 0.01)
#' print(result)
#'
#' # Example 4: Exponential Trend with Default Confidence Level and Summary Data
#' data <- c(1:48)
#' result <- run_seasonality_test(data, trend = "exponential", s = 0.5, summary_data = TRUE)
#' print(result)
#'
#' @export
#' @importFrom stats coef decompose frequency is.ts lm rnorm start stl t.test time ts wilcox.test
#' @importFrom DescTools SignTest
#' @importFrom crayon blue red
#' @importFrom minpack.lm nls.lm.control nlsLM
run_seasonality_test <- function(data, trend=NULL, s=12, confidence_level = 0.05, summary_data = TRUE) {
  summary_data <- summary_data
  if (is.null(trend)) {
    # If trend is NULL, run interactive version of seasonality test
    # #nocov start
    interactive_seasonality_test(data, s, confidence_level, summary_data)
    # #nocov end
  } else {
    # If trend is specified, run regular version of seasonality test
    result <- seasonality_test(data, trend, s, confidence_level)
    print_result <- result
    print_result$data_ts <- NULL
    print_result$trend_not_specified <- NULL
    if (!summary_data) {
      print_result$summary_df <- NULL
    }
    print(print_result)
  }
}

#' Seasonality Test
#'
#' This function performs a seasonality test based on the specified trend and seasonality parameter.
#'
#' @param data The input data for the seasonality test.
#' @param trend The trend type for the seasonality test. Supported options: linear, quadratic, exponential.
#' @param s The seasonality parameter (default = 12).
#' @param confidence_level The desired confidence level for the statistical tests (default = 0.05).
#' @return A list containing the result message, summary statistics, and the time series data.
#' @examples
#' data <- c(1:48)
#' result <- seasonality_test(data, trend = "linear", s = 12, confidence_level = 0.05)
#' print(result)
#' @export
seasonality_test <- function(data, trend=NULL, s=12, confidence_level = 0.05) {
  # Ensure that the data is converted to a time series object
  data <- convert_to_time_series(data, s)

  # Check if confidence_level is between 0 and 1
  if (!is.numeric(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    # #nocov start
    stop("Invalid confidence_level. It should be a number between 0 and 1.")
    # #nocov end
  }

  # If the trend was not specified, call the identify_trend function
  trend_not_specified <- is.null(trend)
  if (trend_not_specified) {
    # #nocov start
    result <- identify_trend(data, s)

    if (is.null(result$trend)) {
      stop("No clear trend detected in the data.")
    }

    data <- result$data_ts
    trend <- result$trend
    # #nocov end
  }

  # Check if s belongs to the supported values if not NULL
  if (!is.null(s) && (!is.character(s) && !is.numeric(s)) || !(s %in% c("weekly", "monthly", "quarterly", "yearly", 52, 12, 4, 1))) {
    # #nocov start
    stop("Invalid value for 's'. Allowed values: 'weekly', 'monthly', 'quarterly', 'yearly' or 52, 12, 4, 1.")
    # #nocov end
  }

  # Check if trend belongs to the supported values if not NULL
  if (!is.null(trend) && !trend %in% c("linear", "quadratic", "exponential")) {
    # #nocov start
    stop("Invalid/Unknown trend. Supported options: linear, quadratic, and exponential")
    # #nocov end
  }

  has_warnings <- FALSE

  if (trend == "linear") {
    # Call the linear function from linear.R
    linear_result <- linear(data)
    Ui <- linear_result$Ui
    Vi <- linear_result$Vi
  } else if (trend == "quadratic") {
    # Call the quadratic function from quadratic.R
    quadratic_result <- quadratic(data, s)
    Ui <- quadratic_result$Ui
    Vi <- quadratic_result$Vi
  } else if (trend == "exponential") {
    # Call the exponential function from exponential.R
    exponential_result <- exponential(data, s)
    Ui <- exponential_result$Ui
    Vi <- exponential_result$Vi
  } else {
    # #nocov start
    stop("Invalid trending curve. Supported options: linear, quadratic, exponential")
    # #nocov end
  }

  # Perform the statistical tests if there are enough observations
  # T Test
  t_test_result <- t.test(Ui, Vi, conf.level = (1-confidence_level))
  p_value_t <- t_test_result$p.value

  # SIGN Test
  sign_test_result <- SignTest(Ui, Vi, conf.level = (1-confidence_level))
  p_value_sign <- sign_test_result$p.value

  # Wilcoxon SR test
  wilcox_test_result <- wilcox.test(Ui, Vi, conf.level = (1-confidence_level), paired = TRUE, exact = FALSE)
  p_value_wilcox <- wilcox_test_result$p.value

  # Check if any of the p-values indicate seasonality
  is_seasonal <- p_value_t < confidence_level || p_value_sign < confidence_level || p_value_wilcox < confidence_level

  # Prepare the results summary
  summary_df <- data.frame(
    Test = c("Student t-Distribution", "Sign Test", "Wilcoxon Signed-Ranks Test"),
    p_value = c(p_value_t, p_value_sign, p_value_wilcox),
    stringsAsFactors = FALSE
  )

  # Prepare the result message
  if (is_seasonal) {
    result_message <- paste("There appears to be statistically significant seasonality in the data at the", 100 - (confidence_level * 100), "% confidence level.")
  } else {
    result_message <- paste("There does not appear to be statistically significant seasonality in the data at the", 100 - (confidence_level * 100), "% confidence level.")
  }

  #summary_data <- summary_data

  # Bundle everything into a list to return
  result <- list(
    message = result_message,
    summary_df = summary_df,
    trend = trend,
    data_ts = if (trend_not_specified) result$data_ts else data,
    trend_not_specified = trend_not_specified,
    has_warnings = has_warnings
  )

  return(result)
}

#' Interactive Seasonality Test
#'
#' This function performs an interactive seasonality test on a given time series data.
#'
#' @param data The input data for the seasonality test.
#' @param s The seasonality parameter (default = 12).
#' @param confidence_level The desired confidence level for the statistical tests (default = 0.05).
#' @param summary_data Flag indicating whether to include the results summary (default = TRUE).
#' @return The result of the seasonality test.
#' @examples
#' data <- c(1:48)
#' result <- interactive_seasonality_test(data, s = 12, confidence_level = 0.05, summary_data = TRUE)
#' print(result)
#' @export
# #nocov start
interactive_seasonality_test <- function(data, s=12, confidence_level = 0.05, summary_data = TRUE) {
  summary_data <- summary_data
  # First, run the seasonality test without specifying a trend
  result <- seasonality_test(data, trend=NULL, s=s, confidence_level=confidence_level)
  result_to_print <- result
  result_to_print$data_ts <- NULL
  result_to_print$trend <- NULL
  result_to_print$trend_not_specified <- NULL

  if (!summary_data) {
    result_to_print$summary_df <- NULL
  }

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
      result <- seasonality_test(data, trend, s=s, confidence_level=confidence_level)

      # Print the result excluding data_ts
      print(result$message)
      print(result$summary_df)

      # Return the result
      return(result)
    } else {
      return(result_to_print)
    }
  } else {
    print(result$message)
    print(result$summary_df)
    return(result)
  }
}
# #nocov end
