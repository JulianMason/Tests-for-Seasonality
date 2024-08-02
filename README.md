# Seasonality Test for Time Series Data: R Package

[![codecov](https://codecov.io/github/JulianMason/Tests-for-Seasonality/graph/badge.svg?token=80ZCI1VDNJ)](https://codecov.io/github/JulianMason/Tests-for-Seasonality) [![R-hub](https://github.com/JulianMason/Tests-for-Seasonality/actions/workflows/rhub.yaml/badge.svg)](https://github.com/JulianMason/Tests-for-Seasonality/actions/workflows/rhub.yaml)

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

### Using Internal Data Examples:
#### Stock Market Indices Data:
```R
# Load and run seasonality tests on S&P 500 data
data(internal_dataset)
sp500_result <- seasonality_test(internal_dataset$sp500_ts, trend = "linear")
print(sp500_result)

# Load and run seasonality tests on NASDAQ data
data(internal_dataset)
nasdaq_result <- seasonality_test(internal_dataset$nq_ts, trend = "linear")
print(nasdaq_result)

# Load and run seasonality tests on Dow Jones data
data(internal_dataset)
dowjones_result <- seasonality_test(internal_dataset$dj_ts, trend = "linear")
print(dowjones_result)
```
#### Weather Data
```R
# Load and run seasonality tests on the GCAG weather data
data(internal_dataset)
gcag_result <- seasonality_test(internal_dataset$gcag_ts, trend = "linear")
print(gcag_result)

```

#### Synthetic Data
```R
# Load and run seasonality tests on synthetic linear seasonal data
data(internal_dataset)
linear_seasonal_result <- seasonality_test(internal_dataset$linear_seasonal, trend = "linear")
print(linear_seasonal_result)

# Load and run seasonality tests on synthetic linear non-seasonal data
data(internal_dataset)
linear_non_seasonal_result <- seasonality_test(internal_dataset$linear_non_seasonal, trend = "linear")
print(linear_non_seasonal_result)

# Load and run seasonality tests on synthetic quadratic seasonal data
data(internal_dataset)
quadratic_seasonal_result <- seasonality_test(internal_dataset$quadratic_seasonal, trend = "quadratic")
print(quadratic_seasonal_result)

# Load and run seasonality tests on synthetic quadratic non-seasonal data
data(internal_dataset)
quadratic_non_seasonal_result <- seasonality_test(internal_dataset$quadratic_non_seasonal, trend = "quadratic")
print(quadratic_non_seasonal_result)

# Load and run seasonality tests on synthetic exponential seasonal data
data(internal_dataset)
exponential_seasonal_result <- seasonality_test(internal_dataset$exponential_seasonal, trend = "exponential")
print(exponential_seasonal_result)

# Load and run seasonality tests on synthetic exponential non-seasonal data
data(internal_dataset)
exponential_non_seasonal_result <- seasonality_test(internal_dataset$exponential_non_seasonal, trend = "exponential")
print(exponential_non_seasonal_result)

```

#### Generic and Anomalous Synthetic Data
```R 
# Display the first few rows and run seasonality test on the generic data
data(internal_dataset)
generic_data_result <- seasonality_test(internal_dataset$generic_data, trend = "linear")
print(generic_data_result)

# Display and run seasonality test on the matrix-format data
data(internal_dataset)
matrix_data_result <- seasonality_test(internal_dataset$mat_data, trend = "linear")
print(matrix_data_result)

# Display and run seasonality test on the matrix-format data (univariate)
data(internal_dataset)
matrix_data_uni_result <- seasonality_test(internal_dataset$mat_data_uni, trend = "linear")
print(matrix_data_uni_result)

# Display and run seasonality test on the anomalous data
data(internal_dataset)
anomalous_data_result <- seasonality_test(internal_dataset$anomalous_data, trend = "linear")
print(anomalous_data_result)

# Display the attributes and run seasonality test on the attributed time series
data(internal_dataset)
attributed_ts_result <- seasonality_test(internal_dataset$attributed_ts, trend = "linear")
print(attributed_ts_result)

# Display and run seasonality test on the non-numeric data
data(internal_dataset)
non_numeric_result <- seasonality_test(internal_dataset$non_numeric, trend = "linear")
print(non_numeric_result)

```

### Input Parameter Customisation Examples
```R
# Custom parameters on linear seasonal data
custom_linear_seasonal_result <- seasonality_test(internal_dataset$linear_seasonal, trend = "linear", s = 12, confidence_level = 0.1)
print(custom_linear_seasonal_result)

# Custom parameters on quadratic seasonal data
custom_quadratic_seasonal_result <- seasonality_test(internal_dataset$quadratic_seasonal, trend = "quadratic", s = 12, confidence_level = 0.05)
print(custom_quadratic_seasonal_result)

# Custom parameters on exponential seasonal data
custom_exponential_seasonal_result <- seasonality_test(internal_dataset$exponential_seasonal, trend = "exponential", s = 12, confidence_level = 0.2)
print(custom_exponential_seasonal_result)
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

