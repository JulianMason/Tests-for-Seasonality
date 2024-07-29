#' Quadratic Seasonality Test
#'
#' This function performs a seasonality test assuming a quadratic trend in the data.
#'
#' @param data A numeric vector or time series object.
#' @param s The frequency of the time series.
#' @return A list containing the Ui, Vi, and Di values.
#' @examples
#' data <- c(1:48)
#' quadratic_result <- quadratic(data, s = 12)
#' @export
#' @importFrom stats ts stl lm

quadratic <- function(data, s) {

  # Decompose the time series using STL
  decomposed_data <- stl(data, s.window="periodic")

  # Extract the trend, seasonality, and noise components
  trend <- as.numeric(decomposed_data$time.series[, "trend"])
  seasonal <- as.numeric(decomposed_data$time.series[, "seasonal"])
  remainder <- as.numeric(decomposed_data$time.series[, "remainder"])

  # De-trended series (seasonal + noise)
  detrended <- seasonal + remainder

  # Calculate c and b
  time_index <- 1:length(data)
  model <- lm(detrended ~ poly(time_index, 2))
  b <- summary(model)$coefficients[2]
  c <- summary(model)$coefficients[3]


  # Calculate Ui and Vi for each year
  n <- length(data) / s
  Ui <- numeric(n)
  Vi <- numeric(n)

  for (i in 1:n) {
    Sj <- detrended[(s*(i-1) + 1):(s*i)]
    C1 <- sum((1:s) * Sj)
    C2 <- sum((1:s)^2 * Sj)

    # Compute Ui
    Ui[i] <- (s*(s+1)/180)*((2*s-1)*(8*s-11)*c^2 - 30*(s-1)*b*c + 15*b^2) +
      (1/(s-1))*(sum(Sj^2) + 2*(b-2*c*s)*C1 + 2*c*C2) +
      ((s^2*(s+1))/3)*(b*c - c^2*(s-1) + (4*c*s*C1)/(s-1))*i +
      ((s^3*(s+1)*c^2)/3)*i^2

    # Compute Vi (without seasonality, so Sj = 0)
    Sj <- rep(0, s) # reset Sj for Vi
    C1 <- sum((1:s) * Sj)
    C2 <- sum((1:s)^2 * Sj)

    Vi[i] <- (s*(s+1)/180)*((2*s-1)*(8*s-11)*c^2 - 30*(s-1)*b*c + 15*b^2) +
      ((s^2*(s+1))/3)*(b*c - c^2*(s-1))*i +
      ((s^3*(s+1)*c^2)/3)*i^2
  }

  # Calculate Di
  Di <- Ui - Vi

  return(list(Ui = Ui, Vi = Vi, Di = Di))
}
