#' Exponential Seasonality Test
#'
#' This function performs a seasonality test assuming an exponential trend in the data.
#'
#' @param data A numeric vector or time series object.
#' @param s The frequency of the time series.
#' @return A list containing the Ui, Vi, and Di values.
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' exponential_result <- exponential(data, s = 12)
#' @export
exponential <- function(data, s = frequency(data)) {
  #requireNamespace(forecast)
  #requireNamespace(minpack.lm)

  # Decompose the time series using STL
  #ts_data <- ts(data, frequency = s)
  #decomposed_data <- stl(ts_data, s.window="periodic")
  decomposed_data <- decompose(data)

  # Extract the seasonality and noise components to get de-trended data
  trend <- decomposed_data$trend
  seasonal <- decomposed_data$seasonal
  remainder <- decomposed_data$random

  # De-trended series (seasonal + noise)
  detrended <- seasonal + remainder

  # Use nlsLM for nonlinear regression
  time_index <- 1:length(data)
  start.list <- list(b = 0.5, c = 0.05)
  model <- nlsLM(detrended ~ b * exp(c * time_index), start = start.list)
  a <- coef(model)["a"]
  b <- coef(model)["b"]
  c <- coef(model)["c"]


  # Calculate Ui and Vi for each year
  n <- length(data) / s
  Ui <- numeric(n)
  Vi <- numeric(n)

  for (i in 1:n) {
    Sj <- detrended[(s*(i-1) + 1):(s*i)]

    # Compute Ui
    term1 <- b^2 * exp(2 * c * ((i - 1) * s + 1))
    term2 <- (1 - exp(2 * c * s)) / (1 - exp(2 * c))
    term3 <- (1 / s) * (1 - exp(c * s)) / (1 - exp(c))
    term4 <- sum(Sj^2)
    term5 <- 2 * b * exp(c * (i - 1) * s) * sum(exp(c * seq_len(s)) * Sj)

    Ui[i] <- term1 * (term2 - term3) + term4 + term5

    # Compute Vi (without seasonal effect)
    Sj <- rep(0, s) # reset Sj for Vi calculation
    termVi <- term1 * (term2 - term3)
    Vi[i] <- termVi
    #Vi[i] <- remainder[i]
  }

  # Calculate Di
  Di <- Ui - Vi

  return(list(Ui = Ui, Vi = Vi, Di = Di))
}

#exponential(exponential_seasonal)

# Seasonal
#exponential(exponential_seasonal, s=12)
#Vi_seasonal <- exponential(exponential_seasonal)$Ui
#Vi_seasonal <- exponential(exponential_seasonal)$Vi
#Di_seasonal <- exponential(exponential_seasonal)$di

# Non seasonal
#exponential(exponential_non_seasonal, s=12)
#Ui_non_seasonal <- exponential(exponential_non_seasonal)$Ui
#Vi_non_seasonal <- exponential(exponential_non_seasonal)$Vi
#Di_non_seasonal <- exponential(exponential_non_seasonal)$di


# T Test
#t.test(Ui_non_seasonal, Vi_non_seasonal, paired = TRUE)
#t.test(Ui_seasonal, Vi_seasonal, paired = TRUE)
#t.test(Di_seasonal, mu=0, alternative="two.sided")

# Sign test
#SIGN.test(Ui_seasonal, Vi_seasonal, md = 0, conf.level = 0.95, alternative = "two.sided")
#SIGN.test(Ui_non_seasonal, Vi_non_seasonal, md = 0, conf.level = 0.95, alternative = "two.sided")


# Wilcoxon
#wilcox.test(Ui_seasonal, Vi_seasonal, paired=TRUE)
#wilcox.test(Ui_non_seasonal, Vi_non_seasonal, paired=TRUE)


#exponential_seasonal_periodic(exponential_seasonal, s=12)
#Ui_seasonal <- exponential(exponential_seasonal)$Ui
#Vi_seasonal <- exponential(exponential_seasonal)$Vi
#Di_seasonal <- exponential(exponential_seasonal)$di

# Non seasonal
#exponential_seasonal_periodic(exponential_non_seasonal, s=12)
#Ui_non_seasonal <- exponential(exponential_non_seasonal)$Ui
#Vi_non_seasonal <- exponential(exponential_non_seasonal)$Vi
#Di_non_seasonal <- exponential(exponential_non_seasonal)$di

