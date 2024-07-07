# Seasonality Test for Time Series Data: R Package

## Overview

This project is an R package developed to detect seasonality in time series data, based on the advanced methodology proposed by Nwogu, Iwueze & Nlebedim (2016). The package simplifies the application of this comprehensive methodology, making it accessible and efficient for users without requiring specialized mathematical skills.

## Features

- **Time Series Conversion**: Converts raw data into time series objects.
- **Trend Detection**: Automatically identifies trends within the data.
- **User Customization**: Allows customization of parameters such as trend type, series frequency, and significance level.
- **Statistical Tests**: Applies a variety of parametric and non-parametric statistical tests to detect seasonality.

## Validation

The package has been empirically validated using both synthetic and real-world datasets, including stock market indices and global air temperatures. The validation demonstrates high accuracy in detecting seasonality, though some limitations remain, particularly in automatic trend detection and test sensitivities.

## Conclusion

This R package bridges the gap in the practical application of advanced seasonality detection methods, offering a robust and user-friendly tool for analysts and researchers.

## Installation

To install the package, use the following command in R:
```R
# Replace 'github_username' with the actual username
devtools::install_github("github_username/seasonality_test")
```

## Usage

### Generating Synthetic Data:
```R
# Load necessary libraries
library(seasonalityTest)
library(ggplot2)

# Function to generate synthetic data
generate_data <- function(type, n, freq, noise_level = 0.1) {
  time <- 1:n
  if (type == "linear") {
    trend <- time
  } else if (type == "quadratic") {
    trend <- time^2
  } else if (type == "exponential") {
    trend <- exp(time / 50)
  } else {
    trend <- rep(0, n)
  }
  
  seasonality <- sin(2 * pi * time / freq)
  noise <- rnorm(n, sd = noise_level)
  
  data <- trend + seasonality + noise
  return(data)
}

# Example data generation
set.seed(123)
linear_data <- generate_data("linear", 100, 12)
quadratic_data <- generate_data("quadratic", 100, 12)
exponential_data <- generate_data("exponential", 100, 12)
no_seasonality_data <- generate_data("none", 100, 12)

```
### Example Plots
```R
# Plot the data
plot_data <- function(data, title) {
  df <- data.frame(time = 1:length(data), value = data)
  ggplot(df, aes(x = time, y = value)) + 
    geom_line() + 
    ggtitle(title) + 
    theme_minimal()
}

plot_data(linear_data, "Linear Trend with Seasonality")
plot_data(quadratic_data, "Quadratic Trend with Seasonality")
plot_data(exponential_data, "Exponential Trend with Seasonality")
plot_data(no_seasonality_data, "No Seasonality")
```

### Running Seasonaity Tests
```R 
# Run seasonality tests on synthetic data
linear_result <- seasonality_test(linear_data, trend = "linear")
quadratic_result <- seasonality_test(quadratic_data, trend = "quadratic")
exponential_result <- seasonality_test(exponential_data, trend = "exponential")
no_seasonality_result <- seasonality_test(no_seasonality_data, trend = "linear")

print(linear_result)
print(quadratic_result)
print(exponential_result)
print(no_seasonality_result)
```

### Input Parameter Customisation Examples
```R
# Custom parameters
custom_result <- seasonality_test(linear_data, trend = "linear", s = 12, confidence_level = 0.1)
print(custom_result)

```

### Edge Cases
```R
# Edge case: Short time series
short_data <- c(10, 15)
short_result <- seasonality_test(short_data, trend = "linear")
print(short_result)

# Edge case: Non-numeric input
non_numeric_data <- c("a", "b", "c")
non_numeric_result <- seasonality_test(non_numeric_data, trend = "linear")
print(non_numeric_result)

# Edge case: Missing values
missing_values_data <- c(10, 20, NA, 30, 40)
missing_values_result <- seasonality_test(missing_values_data, trend = "linear")
print(missing_values_result)

```



## Author

Julian Dein Bara-Mason

## License

This project is licensed under the MIT License.

