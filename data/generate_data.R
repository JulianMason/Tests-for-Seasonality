# Load required libraries
library(stats)
require(stats)

# Synthetic Data Generation Function
generate_timeseries <- function(trend, seasonality = TRUE, noise = TRUE) {
  n <- 600  # 10 years of monthly data
  noise_component <- ifelse(noise, rnorm(n, 0, 3), rep(0, n))
  if (trend == "linear") {
    if (seasonality) {
      ts_data <- ts(seq(1, n) + 10 * sin(seq(1, n) * 2 * pi / 12) + noise_component, frequency = 12)
    } else {
      ts_data <- ts(seq(1, n) + noise_component, frequency = 12)
    }
  } else if (trend == "quadratic") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      ts_data <- ts(seq(1, n)^2 + 500 * sin(seq(1, n) * 2 * pi / 12) + noise_component, frequency = 12)
    } else {
      ts_data <- ts(seq(1, n)^2 + noise_component, frequency = 12)
    }
  } else if (trend == "exponential") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      ts_data <- ts(2^seq(1, n) + 100 * sin(seq(1, n) * 2 * pi / 12) + noise_component, frequency = 12)
    } else {
      ts_data <- ts(2^seq(1, n) + noise_component, frequency = 12)
    }
  } else {
    stop("Invalid trend specified.")
  }
  return(ts_data)
}

# Generate synthetic datasets
linear_seasonal <- generate_timeseries("linear")
linear_non_seasonal <- generate_timeseries("linear", seasonality = FALSE, noise = TRUE)
quadratic_seasonal <- generate_timeseries("quadratic")
quadratic_non_seasonal <- generate_timeseries("quadratic", seasonality = FALSE, noise = TRUE)
exponential_seasonal <- generate_timeseries("exponential")
exponential_non_seasonal <- generate_timeseries("exponential", seasonality = FALSE, noise = TRUE)

# 4. Generic Dataset
generic_data <- rnorm(1200, mean = 50, sd = 10)

# 5. Matrix-format Dataset
mat_data <- matrix(rnorm(120 * 2, mean = 50, sd = 10), ncol = 2)
colnames(mat_data) <- c("Series1", "Series2")
mat_data_uni <- matrix(rnorm(120 * 2, mean = 50, sd = 10), ncol = 1)
colnames(mat_data_uni) <- c("Series1")

# 6. Data with Anomalies
anomalous_data <- rnorm(120, mean = 50, sd = 10)
anomalous_data[sample(1:120, 5)] <- NA
anomalous_data[sample(1:120, 5)] <- Inf

# 7. Attributed Time Series
attributed_ts <- ts(rnorm(120, mean = 50, sd = 10), frequency = 12)
attributes(attributed_ts)$src <- "Synthetic Generator"
attributes(attributed_ts)$updated <- Sys.Date()
attributes(attributed_ts)$index <- sample(1:1000, 120)

# 8. Non-Numeric Data
non_numeric <- factor(c("Red", "Blue", "Green", "Red", "Blue"))

# 9. Weather Data
# Example synthetic weather data
gcag_data <- data.frame(
  Date = seq(as.Date("2000-01-01"), as.Date("2022-12-31"), by = "month"),
  Mean = rnorm(276, mean = 50, sd = 10)
)
gcag_data$Year <- as.numeric(format(gcag_data$Date, "%Y"))
gcag_data$Month <- as.numeric(format(gcag_data$Date, "%m"))
gcag_ts <- ts(gcag_data$Mean, start = c(min(gcag_data$Year), min(gcag_data$Month)), frequency = 12)

# Combine all datasets into a list
internal_dataset <- list(
  linear_seasonal = linear_seasonal,
  linear_non_seasonal = linear_non_seasonal,
  quadratic_seasonal = quadratic_seasonal,
  quadratic_non_seasonal = quadratic_non_seasonal,
  exponential_seasonal = exponential_seasonal,
  exponential_non_seasonal = exponential_non_seasonal,
  generic_data = generic_data,
  mat_data = mat_data,
  mat_data_uni = mat_data_uni,
  anomalous_data = anomalous_data,
  attributed_ts = attributed_ts,
  non_numeric = non_numeric,
  gcag_ts = gcag_ts
)

# Save the internal dataset to a file
save(internal_dataset, file = "data/internal_dataset.RData")
