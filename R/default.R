convert_to_time_series <- function(data, s="monthly") {

  # Check if data is numeric and not empty
  if (!is.numeric(data) || length(data) == 0) {
    stop("Data should be numeric and non-empty.")
  }

  # Check for NA or Inf values in data
  if (any(is.na(data)) || any(is.infinite(data))) {
    stop("Data cannot contain NA or Inf values.")
  }

  # If the data is already a time series, simplify and return it
  if (is.ts(data)) {
    return(ts(as.numeric(data), start = start(data), frequency = frequency(data)))
  }

  # Flatten in case of matrix or dataframe with one column
  if (is.matrix(data) || is.data.frame(data)) {
    if (ncol(data) == 1) {
      data <- as.numeric(data[, 1])
    } else {
      stop("Multivariate data provided. This function expects a univariate series.")
    }
  }

  # Check if s is a character or numeric and if it belongs to the supported values
  allowed_s <- c("weekly", "monthly", "quarterly", "yearly", 52, 12, 4, 1)
  if (!s %in% allowed_s) {
    stop("Invalid value for 's'. Allowed values: 'weekly', 'monthly', 'quarterly', 'yearly' or 52, 12, 4, 1.")
  }

  # Convert s to a string if it's numeric
  if (is.numeric(s)) {
    s <- switch(as.character(s),
                "1" = "yearly",
                "4" = "quarterly",
                "12" = "monthly",
                "52" = "weekly",
                stop("Invalid numeric frequency specified."))
  }

  s <- tolower(s)

  frequency <- switch(s,
                      weekly = 52,
                      monthly = 12,
                      quarterly = 4,
                      yearly = 1,
                      stop("Invalid frequency specified."))

  if (length(data) < 2 * frequency) {
    stop("Insufficient amount of data. Need sufficient data for at least 2 complete chosen seasons")
  }

  data <- ts(data, frequency = frequency)

  # Simplify the data to remove any potential leftover attributes
  data <- ts(as.numeric(data), start = start(data), frequency = frequency)

  return(data)
}

identify_trend <- function(data, s="monthly") {

  if ((!is.character(s) && !is.numeric(s)) ||
      !(s %in% c("weekly", "monthly", "quarterly", "yearly", 52, 12, 4, 1))) {
    stop("Invalid value for 's'. Allowed values: 'weekly', 'monthly', 'quarterly', 'yearly' or 52, 12, 4, 1.")
  }


  data_ts <- convert_to_time_series(data, s)
  index <- as.numeric(time(data_ts))

  cubic_model <- lm(data_ts ~ poly(index, 3))

  p_values <- summary(cubic_model)$coefficients[,4]

  check_significance <- function(p_value) {
    if (p_value < 0.001) {
      return("a")
    } else if (p_value < 0.01) {
      return("b")
    } else if (p_value < 0.05) {
      return("c")
    } else if (p_value < 0.1) {
      return("d")
    } else {
      return("e")
    }
  }

  significance_levels <- sapply(p_values[-1], check_significance)  # Excluding the intercept

  if (all(significance_levels == "e")) {
    trend <- "unknown"
  } else if (all(significance_levels == significance_levels[1])) {
    trend <- "exponential"
  } else if (significance_levels[1] == significance_levels[2]) {
    trend <- "quadratic"
  } else {
    trend <- "linear"
  }

  return(list("trend" = trend, "data_ts" = data_ts))
}
