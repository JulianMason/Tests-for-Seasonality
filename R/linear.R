#' Linear Seasonality Test
#'
#' This function performs a seasonality test assuming a linear trend in the data.
#'
#' @param data A numeric vector or time series object.
#' @param s The frequency of the time series.
#' @param seasons_to_check A vector of seasons to check for seasonality.
#' @return A list containing the Ui, Vi, and di values.
#' @examples
#' data <- ts(c(1:48), frequency = 12)
#' linear_result <- linear(data, s = 12)
#' @export
#' @importFrom stats ts stl
linear <- function(data, s = frequency(data), seasons_to_check = 1:s) {
  #requireNamespace(forecast)

  # Decompose the time series using STL
  ts_data <- ts(data, frequency = s)
  decomposed_data <- stl(ts_data, s.window="periodic")

  # Extract the trend, seasonal, and random components
  trend <- as.numeric(decomposed_data$time.series[, "trend"])
  seasonal <- as.numeric(decomposed_data$time.series[, "seasonal"])
  remainder <- as.numeric(decomposed_data$time.series[, "remainder"])

  # De-trended series (seasonal + noise)
  detrended <- seasonal + remainder

  # Initialize Ui, Vi, and di
  Ui <- numeric(length(seasons_to_check))
  Vi <- numeric(length(seasons_to_check))
  di <- numeric(length(seasons_to_check))

  # Calculate Ui and Vi for each specified seasonal period
  for (index in 1:length(seasons_to_check)) {
    i <- seasons_to_check[index]
    S_j <- detrended[seq(i, length(data), by = s)]
    j <- 1:length(S_j)

    # Only proceed if S_j has a sufficient size (at least two non-NA values)
    if (sum(!is.na(S_j)) > 1) {
      b <- 0

      if (s != 1) {  # Prevent division by zero when s == 1
        Ui[index] <- (b^2 * (s * (s + 1) / 12)) + (2 * b / (s - 1)) * sum(j * S_j, na.rm = TRUE) + (1 / (s - 1)) * sum(S_j^2, na.rm = TRUE)
      } else {
        # #nocov start
        Ui[index] <- NA
        # #nocov end
      }

      # Vi is the random component for that seasonal period
      Vi[index] <- remainder[i]

      # Calculate di
      di[index] <- Ui[index] - Vi[index]
    } else {
      # #nocov start
      Ui[index] <- NA
      Vi[index] <- NA
      di[index] <- NA
      # #nocov end
    }
  }

  return(list(Ui = Ui, Vi = Vi, di = di))
}
