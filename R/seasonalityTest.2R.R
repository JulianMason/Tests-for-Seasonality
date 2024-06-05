#' Perform Seasonality Test
#'
#' This function performs seasonality tests on the given data based on the specified trend.
#'
#' @param data The input data for the seasonality test.
#' @param trend The trend type for the seasonality test. Supported options: linear, quadratic, exponential, linearMA, linearEXP.
#' @param window The window size for the linear moving averages trend (only applicable when trend = "linearMA").
#' @param s The smoothing parameter for exponential trend (only applicable when trend = "quadratic", "exponential", or "linearEXP").
#' @param confidence_level The desired confidence level for the statistical tests (default = 0.05).
#' @param summary_data Flag indicating whether to include the results summary (default = NULL).
#' @return If summary_data is NULL, returns a character string indicating the presence or absence of statistically significant seasonality in the data.
#'         If summary_data is provided, returns a list with the following elements:
#'           - is_seasonal: A logical value indicating the presence or absence of statistically significant seasonality in the data.
#'           - summary_data: A data frame containing the test names and p-values.
#'           - t_summary: Summary statistics for the Student t-Distribution test.
#'           - sign_summary: Summary statistics for the Sign Test.
#'           - wilcox_summary: Summary statistics for the Wilcoxon Signed-Ranks Test.
#' @examples
#' # Example 1: Linear Trend with default Confidence Level and Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "linear", summary_data = TRUE)
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
#' # Example 4: Linear Moving Averages Trend with Different Confidence Level and Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "linearMA", window = 3, s = 1, confidence_level = 0.001, summary_data = TRUE)
#' print(result)
#'
#' # Example 5: Linear Exponential Smoothing Trend with Default Confidence Level and Summary Data
#' data <- c(10, 15, 20, 15, 10)
#' result <- seasonality_test(data, trend = "linearEXP", s = 0.1, summary_data = TRUE)
#' print(result)
#'
#' @export
# Function to adjust p-value representation
seasonality_test <- function(data, trend=NULL, s=NULL, seasons_to_check = NULL, confidence_level = 0.05, summary_data = NULL) {

  warnings_message <- NULL

  # Define a variable to track if the trend was not specified
  trend_not_specified <- is.null(trend)

  # Check if trend is NULL
  if (trend_not_specified) {
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/default.R")
    result <- identify_trend(data, period)

    if (is.null(result$trend)) {
      stop("No clear trend detected in the data.")
    }

    data <- result$data_ts
    trend <- result$trend
  }

  n <- length(data)
  has_warnings <- FALSE  # Initialize a variable to track if warnings occurred

  if (trend == "linear") {
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/linear.R")
    linear_result <- linear(data)
    di <- linear_result$di
    Ui <- linear_result$Ui
    Vi <- linear_result$Vi
  } else if (trend == "quadratic") {
    if (is.null(s)) {
      stop("For the quadratic trend, the 's' parameter specifying the number of seasons must be provided.")
    }
    source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/quadratic.R")
    quadratic_result <- quadratic(data, s)
    di <- quadratic_result$di
    Ui <- quadratic_result$Ui
    Vi <- quadratic_result$Vi
  } else if (trend == "exponential") {
    withCallingHandlers({
      source("/Users/jay/Documents/Documents - Jay's Macbook Pro (13281)/MSc Data Science & Analytics - UoL/Dissertation/disso 2/R/exponential.R")
      exponential_result <- exponential(data, s)
      di <- exponential_result$di
      Ui <- exponential_result$Ui
      Vi <- exponential_result$Vi
    }, warning = function(w) {
      has_warnings <<- TRUE  # Set has_warnings to TRUE if any warning occurs
      warnings_message <<- as.character(w$message)
      invokeRestart("muffleWarning")  # This will suppress the original warning messages
    })
  } else {
    stop("Invalid trending curve. Supported options: linear, quadratic, exponential, linearMA, linearEXP.")
  }

  # Perform paired t-test between Ui and Vi
  t_test_result <- t.test(Ui, Vi, paired = TRUE)
  p_value_t <- t_test_result$p.value
  t_statistic <- t_test_result$statistic


  # Sign test for paired data between Ui and Vi
  signs <- sign(Ui - Vi)
  n_plus <- sum(signs > 0)
  n_minus <- sum(signs < 0)

  #p_value_sign <- binom.test(min(n_plus, n_minus), length(signs), 0.5, alternative = "two.sided")$p.value
  if (n_plus == 0 && n_minus == 0) {
    p_value_sign <- NA  # Assign NA or another placeholder value
  } else if (is.integer(n_plus) && n_plus >= 0 && is.integer(n_minus) && n_minus >= 0) {
    p_value_sign <- binom.test(min(n_plus, n_minus), length(signs), 0.5, alternative = "two.sided")$p.value
  } else {
    stop("Invalid values for n_plus or n_minus.")
  }

  # Wilcoxon Signed-Rank test for paired data between Ui and Vi
  #suppressWarnings({
    #wilcox_test_result <- wilcox.test(Ui, Vi, paired = TRUE)
  #})

  wilcox_test_result <- wilcox.test(Ui, Vi, paired = TRUE, exact = FALSE)
  p_value_wilcox <- wilcox_test_result$p.value
  wilcox_statistic <- wilcox_test_result$statistic

  # Decision on seasonality
  if (is.na(p_value_sign)) {
    is_seasonal <- FALSE  # Non-seasonal if sign test is indeterminate. Affects ALL below
  } else {
    is_seasonal <- all(p_value_t < confidence_level, p_value_sign < confidence_level, p_value_wilcox < confidence_level)
  }

  if (is.null(summary_data)) {
    if (is_seasonal) {
      return("There appears to be statistically significant seasonality in the data")
    } else {
      return("There does not appear to be statistically significant seasonality in the data")
    }
  } else {
    t_summary <- list(
      Test = "Student t-Distribution",
      t_statistic = t_statistic,
      p_value = p_value_t
    )
    sign_summary <- list(
      Test = "Sign Test",
      n_plus = n_plus,
      n_minus = n_minus,
      test_statistic = min(n_plus, n_minus),
      p_value = p_value_sign
    )
    wilcox_summary <- list(
      Test = "Wilcoxon Signed-Ranks Test",
      test_statistic = wilcox_statistic,
      p_value = p_value_wilcox
    )
    summary_data <- list(t_summary, sign_summary, wilcox_summary)

    summary_list <- list(
      is_seasonal = is_seasonal,
      summary_data = summary_data,
      confidence_level = confidence_level,
      has_warnings = has_warnings
    )

    # Construct summary_data dataframe to hold results
    #summary_data <- data.frame(
      #Test = c("Student t-Distribution", "Sign Test", "Wilcoxon Signed-Ranks Test"),
      #p_value = c(p_value_t, p_value_sign, p_value_wilcox),
      #p_value = c(t_summary$p_value, sign_summary$p_value, wilcox_summary$p_value),
      #test_stat = c(t_statistic, min(sign_summary$n_plus, sign_summary$n_minus), wilcox_statistic),
      #n_plus = c(NA, sign_summary$n_plus, NA),
      #n_minus = c(NA, sign_summary$n_minus, NA),
      #stringsAsFactors = FALSE
    #)

    # Check for seasonality and generate a result message
    if(is_seasonal){
      result_message <- paste("There appears to be statistically significant seasonality in the data at the", confidence_level*100, "% confidence level.")
    } else {
      result_message <- paste("There does not appear to be statistically significant seasonality in the data at the", confidence_level*100, "% confidence level.")
    }



    # Bundle everything into a list to return
    #result <- list(
      #is_seasonal = is_seasonal,
      #has_warnings = !is.null(warnings_message),
      #message = result_message,
      #summary_data = summary_data,
      #confidence_level = confidence_level,
      #trend_not_specified = trend_not_specified
    #)
    class(summary_list) <- "summary_seasonalityTest"
    return(summary_list)
    #class(result) <- "summary_seasonalityTest"
    #return(result)
  }
}


interactive_seasonality_test <- function(data, trend=NULL, window=NULL, s=NULL, seasons_to_check = NULL, confidence_level = 0.05, summary_data = TRUE) {

  # Initial run of the seasonality test
  results <- seasonality_test(data, trend, window, s, seasons_to_check, confidence_level, summary_data)

  # Print the initial result using the provided print function
  #print.summary_seasonalityTest(results)

  # Only proceed with the interactive step if trend is not specified
  if (is.null(trend)) {
    # Display the warning
    cat("!! WARNING - UNRELIABLE RESULT: You did not specify a trend type. For greater accuracy, please specify a trend.\n")
    cat(paste("Identified trend: ", results$identified_trend, "\n"))

    # Ask the user if they'd like to improve results by visually inspecting the trend
    response <- readline(prompt = "Would you like to visually inspect the trend and identify the most suitable trend? (yes/no): ")

    if (tolower(response) == "yes") {
      # Decompose the data and plot the trend component
      decomposed <- decompose(data)
      plot(decomposed$trend, main = "Trend Component of Time Series Data", ylab = "Trend")

      # Ask for their trend choice
      trend <- readline(prompt = "Inspect the plot of the trend and select the most suitable trend (linear, quadratic, exponential): ")

      # Rerun the test with the specified trend
      results_updated <- seasonality_test(data, trend, window, s, seasons_to_check, confidence_level, summary_data)

      # Print updated results using the provided print function
      print.summary_seasonalityTest(results_updated)

      return(results_updated)
    }
  }

  return(results)
}

run_seasonality_test <- function(data, trend=NULL, window=NULL, s=NULL, confidence_level = 0.05, summary_data = TRUE) {
  results <- seasonality_test(data, trend, window, s, seasons_to_check, confidence_level, summary_data)
  result_to_print <- results
  result_to_print$data <- NULL
  result_to_print$trend <- NULL
  #result_to_print$trend_not_specified <- NULL
  print(result_to_print)
  print.summary_seasonalityTest(result_to_print)

  if (result_to_print$trend) {
    # If trend is NULL, run interactive version of seasonality test
    interactive_seasonality_test(data, period, window, s, confidence_level, summary_data)
  } else {
    # If trend is specified, run regular version of seasonality test
    print.summary_seasonalityTest(results)
  }
}

# Print Summary
print.summary_seasonalityTest <- function(x) {
  cat(rep("-", 60), "\n")
  if (x$is_seasonal) {
    cat("There appears to be statistically significant seasonality in the data, at the", 100-(x$confidence_level * 100), "% confidence level\n")
  } else {
    cat("There does not appear to be statistically significant seasonality in the data, at the", 100-(x$confidence_level * 100), "% confidence level\n")
  }
  cat(rep("-", 60), "\n")

  if (!is.null(x$summary_data)) {
    cat("Test Summary:\n\n")
    for (test in x$summary_data) {
      cat("Test:", test$Test, "\n")
      if (test$Test == "Student t-Distribution") {
        # Assuming t-statistic will be present only for Student's t-distribution
        cat("t-statistic:", test$t_statistic, "\n")
      }
      if (test$Test != "Student t-Distribution") {
        cat("Test Statistic:", test$test_statistic, "\n")
      }
      if (test$Test == "Sign Test") {
        cat("n_plus:", test$n_plus, "\n")
        cat("n_minus:", test$n_minus, "\n")
      }
      if (is.na(test$p_value)) {
        cat("p-value: NA (Manual override due to constant data)\n")
      } else if (test$p_value == 0) {
        cat("p-value: <", .Machine$double.eps, "\n\n")
      } else {
        cat("p-value:", test$p_value, "\n\n")
      }
    }
  }

  if (x$has_warnings) {
    warning("WARNING: The data may not have exponential patterns:")
  }

  invisible(x)
}

########################################## REAL WORLD DATA TEST############################################
# Load the quantmod library for retrieving financial data
library(seasonalityTest)
library(quantmod)

# Define the ticker symbol for the S&P 500 (^GSPC)
ticker <- "^GSPC"

# Set the start and end dates for the data
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2022-12-31")

# Retrieve the S&P 500 index data using quantmod
getSymbols(ticker, from = start_date, to = end_date)

# Access the S&P 500 index data using the ticker symbol as an object
sp500 <- GSPC$GSPC.Close

# Convert the stock prices into a time series object
sp500_ts <- ts(sp500, start = start_date, end = end_date, frequency = 12)

###### TESTS
# Linear
seasonality_test(sp500_ts, "linear", seasons_to_check = 1, summary_data = TRUE)

# Quadratic
seasonality_test(sp500_ts, "quadratic", s=12, summary_data = TRUE)

# Exponential
seasonality_test(sp500_ts, "exponential", s=12, summary_data = TRUE)

########################################### SYNTHETIC DATA TESTS ###########################################

install.packages(c("forecast", "tibble", "ggplot2"))
library(forecast)
library(tibble)
library(ggplot2)

############# Data Generation
generate_timeseries <- function(trend, seasonality = TRUE, noise = TRUE) {
  n <- 120  # Two years of monthly data

  noise_component <- ifelse(noise, rnorm(n, 0, 3), rep(0, n))

  if (trend == "linear") {
    if (seasonality) {
      # Linear with seasonality
      ts_data <- ts(seq(1, n) + 10*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Linear without seasonality
      ts_data <- ts(seq(1, n) + noise_component, frequency = 12)
    }
  } else if (trend == "quadratic") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      # Quadratic with seasonality
      ts_data <- ts(seq(1, n)^2 + 500*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Quadratic without seasonality
      ts_data <- ts(seq(1, n)^2 + noise_component, frequency = 12)
    }
  } else if (trend == "exponential") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      # Exponential with seasonality
      ts_data <- ts(2^seq(1, n) + 100*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Exponential without seasonality
      ts_data <- ts(2^seq(1, n) + noise_component, frequency = 12)
    }
  } else {
    stop("Invalid trend specified.")
  }

  return(ts_data)
}

# Linear Data Generation
linear_seasonal <- generate_timeseries("linear") # With Seasonality
linear_non_seasonal <- generate_timeseries("linear", seasonality = FALSE, noise = TRUE) # No Seasonality
#linear_trend <- generate_timeseries("linear", seasonality = FALSE, noise = FALSE) # Trend only

# Quadratic Data Generation
quadratic_seasonal <- generate_timeseries("quadratic") # With Seasonality
quadratic_non_seasonal <- generate_timeseries("quadratic", seasonality = FALSE, noise = TRUE) # No Seasonality
quadratic_trend <- generate_timeseries("quadratic", seasonality = FALSE, noise = FALSE) # Trend only

# Exponential Data Generation
exponential_seasonal <- generate_timeseries("exponential") # With Seasonality
exponential_non_seasonal <- generate_timeseries("exponential", seasonality = FALSE, noise = TRUE) # No Seasonality
exponential_trend <- generate_timeseries("exponential", seasonality = FALSE, noise = FALSE) # Trend only

######################## Linear tests #############################
seasonality_test(linear_seasonal, "linear", seasons_to_check = 1, summary_data = TRUE) # With Seasonality
interactive_seasonality_test(linear_seasonal, summary_data = TRUE)
seasonality_test(linear_non_seasonal, "linear", seasons_to_check = 1:3, summary_data = TRUE)
#seasonality_test(linear_trend, "linear", summary_data = TRUE)

ui_vi_linear(linear_seasonal)
ui_vi_linearEXP(linear_seasonal)
ui_vi_linearEXP(linear_non_seasonal)

seasonality_test(linear_seasonal, "linearEXP", summary_data = TRUE) # With Seasonality
seasonality_test(linear_non_seasonal, "linearEXP", summary_data = TRUE)

# Plots
autoplot(linear_seasonal) + ggtitle("Linear with Seasonality")
autoplot(linear_non_seasonal) + ggtitle("Linear without Seasonality")
#autoplot(linear_trend) + ggtitle("Linear without Seasonality & Noise")

# Ui, Vi, Di values
seasonal_result <- ui_vi_linear(linear_seasonal)
non_seasonal_result <- ui_vi_linear(linear_non_seasonal)
ui_vi_linear(linear_seasonal)

# T Test
t.test(non_seasonal_result$Ui, non_seasonal_result$Vi, paired = TRUE)
t.test(seasonal_result$Ui, seasonal_result$Vi, paired = TRUE)
print(t_test_result$p.value)

# Sign test
# If not installed, install the package
# install.packages("BSDA")
library(BSDA)

SIGN.test(seasonal_result$Ui, seasonal_result$Vi, md = 0, conf.level = 0.95, alternative = "two.sided")
SIGN.test(non_seasonal_result$Ui, non_seasonal_result$Vi, md = 0, conf.level = 0.95, alternative = "two.sided")


# Wilcoxon
wilcox.test(seasonal_result$Ui, seasonal_result$Vi, paired=TRUE)
wilcox.test(non_seasonal_result$Ui, non_seasonal_result$Vi, paired=TRUE)
print(wilcox_test_result)



######################## Quadratic tests #############################
seasonality_test(quadratic_seasonal, "quadratic", s=frequency(quadratic_seasonal), summary_data = TRUE) # With Seasonality
seasonality_test(quadratic_non_seasonal, "quadratic", s=12, summary_data = TRUE)
seasonality_test(quadratic_trend, "quadratic", s=12, summary_data = TRUE)

# Plots
autoplot(quadratic_seasonal) + ggtitle("Quadratic with Seasonality")
autoplot(quadratic_non_seasonal) + ggtitle("Quadratic without Seasonality")
#autoplot(quadratic_trend) + ggtitle("Quadratic without Seasonality & Noise")

# Ui, Vi, Di values
quad_res_seasonal <- Ui_vi_quadratic(quadratic_seasonal, s=12)
quad_non_seasonal <- Ui_vi_quadratic(quadratic_non_seasonal, s=12)
print(quad_res_seasonal)
print(quad_non_seasonal)

######################## Exponential tests #############################
seasonality_test(exponential_seasonal, "exponential", s=12, summary_data = TRUE) # With Seasonality
seasonality_test(exponential_non_seasonal, "exponential", s=12, summary_data = TRUE)
#seasonality_test(exponential_trend, "exponential", s=12, summary_data = TRUE)

head(exponential_seasonal)
head(exponential_non_seasonal)

# Plots
autoplot(exponential_seasonal) + ggtitle("Exponential with Seasonality")
autoplot(exponential_non_seasonal) + ggtitle("Exponential without Seasonality")
#autoplot(exponential_trend) + ggtitle("Quadratic without Seasonality & Noise")

# Ui, Vi, Di values
ex_res_seasonal <- exponential(exponential_seasonal, s=12)
ex_non_seasonal <- exponential(exponential_non_seasonal, s=12)
print(ex_res_seasonal)
print(ex_non_seasonal)

decomposed_seasonal <- decompose(exponential_seasonal)
decomposed_seasonal$
plot(decomposed_seasonal)

decomposed_non_seasonal <- decompose(exponential_non_seasonal)
plot(decomposed_non_seasonal)

