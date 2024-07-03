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

Basic usage examples:
```R
library(seasonalityTest)

# Example data
data <- c(10, 15, 20, 15, 10, 25, 30, 20, 15, 10)

# Run seasonality test with automatic trend detection
result <- run_seasonality_test(data)
print(result)

# Run seasonality test with specified trend
result <- run_seasonality_test(data, trend = "linear")
print(result)
```



## Author

Julian Dein Bara-Mason

## License

This project is licensed under the MIT License.

