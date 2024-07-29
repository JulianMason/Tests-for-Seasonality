#' Exponential Seasonality Test
#'
#' This function performs a seasonality test assuming an exponential trend in the data.
#'
#' @param data A numeric vector or time series object.
#' @param s The frequency of the time series.
#' @return A list containing the Ui, Vi, and Di values.
#' @examples
#' data <- c(1:48)
#' exponential_result <- exponential(data, s = 12)
#' @export
#' @importFrom stats ts decompose coef
#' @importFrom minpack.lm nls.lm.control nlsLM

exponential <- function(data, s = frequency(data)) {
  # Decompose the time series using STL
  decomposed_data <- decompose(data)

  # Extract the seasonality and noise components to get de-trended data
  trend <- decomposed_data$trend
  seasonal <- decomposed_data$seasonal
  remainder <- decomposed_data$random

  # De-trended series (seasonal + noise)
  detrended <- seasonal + remainder

  # Use nlsLM for nonlinear regression with increased maxiter
  time_index <- 1:length(data)
  start.list <- list(b = 0.5, c = 0.05)
  control.list <- nls.lm.control(maxiter = 1000) # Increased from 50
  model <- nlsLM(detrended ~ b * exp(c * time_index), start = start.list, control = control.list)

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
  }

  # Calculate Di
  Di <- Ui - Vi

  return(list(Ui = Ui, Vi = Vi, Di = Di))
}
