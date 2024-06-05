linear <- function(data, s = frequency(data), seasons_to_check = 1:s) {
  require(forecast)

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
        Ui[index] <- NA
      }

      # Vi is the random component for that seasonal period
      Vi[index] <- remainder[i]

      # Calculate di
      di[index] <- Ui[index] - Vi[index]
    } else {
      Ui[index] <- NA
      Vi[index] <- NA
      di[index] <- NA
    }
  }

  return(list(Ui = Ui, Vi = Vi, di = di))
}
