library(quantmod)

ticker_sp <- "^GSPC"
ticker_nq <- "^IXIC"
ticker_dj <- "^DJI"

# Set the start and end dates for the data
start_date <- as.Date("1980-01-01")
end_date <- as.Date("2022-12-31")

# Retrieve the S&P 500 index data using quantmod
getSymbols(ticker_sp, from = start_date, to = end_date)
getSymbols(ticker_nq, from = start_date, to = end_date)
getSymbols(ticker_dj, from = start_date, to = end_date)

# Access the S&P 500 index data using the ticker symbol as an object
sp500 <- GSPC$GSPC.Close
nq <- IXIC$IXIC.Close
dj <- DJI$DJI.Close

# Calculate the start and end years
start_year <- as.numeric(format(start_date, "%Y"))
end_year <- as.numeric(format(end_date, "%Y"))

# Convert the stock prices into a time series object with daily frequency
sp500__ts <- ts(sp500, start = c(start_year, 1), end = c(end_year, 12), frequency = 12)
nq__ts <- ts(nq, start = c(start_year, 1), end = c(end_year, 12), frequency = 12)
dj__ts <- ts(dj, start = c(start_year, 1), end = c(end_year, 12), frequency = 12)

length(sp500__ts)

# Plot S&P 500 Time Series
plot(sp500__ts, main="Time Series Plot of S&P 500", xlab="Date", ylab="Closing Price", col="black", lwd=2)

# Plot NASDAQ Time Series
plot(nq__ts, main="Time Series Plot of NASDAQ", xlab="Date", ylab="Closing Price", col="black", lwd=2)

# Plot DOW Jones Time Series
plot(dj__ts, main="Time Series Plot of DOW Jones", xlab="Date", ylab="Closing Price", col="black", lwd=2)



############### SYNTHETIC DATA
generate_timeseries <- function(trend, seasonality = TRUE, noise = TRUE) {
  n <- 600  # 10 years of monthly data

  noise_component <- ifelse(noise, rnorm(n, 0, 3), rep(0, n))

  if (trend == "linear") {
    if (seasonality) {
      # Linear with seasonality
      ts_data <- ts(seq(1, n) + 10*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Linear without seasonality
      ts_data <- ts(seq(1, n) + noise_component, frequency = 12)
    }
  } else if (trend == "quadratic") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      # Quadratic with seasonality
      ts_data <- ts(seq(1, n)^2 + 500*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Quadratic without seasonality
      ts_data <- ts(seq(1, n)^2 + noise_component, frequency = 12)
    }
  } else if (trend == "exponential") {
    noise_component <- ifelse(noise, rnorm(n, 0, 50), rep(0, n))
    if (seasonality) {
      # Exponential with seasonality
      ts_data <- ts(2^seq(1, n) + 100*sin(seq(1, n)*2*pi/12) + noise_component, frequency = 12)
    } else {
      # Exponential without seasonality
      ts_data <- ts(2^seq(1, n) + noise_component, frequency = 12)
    }
  } else {
    stop("Invalid trend specified.")
  }

  return(ts_data)
}

# Linear Data Generation
linear_seasonal <- generate_timeseries("linear") # With Seasonality
linear_non_seasonal <- generate_timeseries("linear", seasonality = FALSE, noise = TRUE) # No Seasonality

plot(linear_seasonal, main="Time Series Plot of Linear Seasonal", xlab="Year", ylab="Value", col="black", lwd=2)
plot(linear_non_seasonal, main="Time Series Plot of Linear Non-Seasonal", xlab="Year", ylab="Value", col="black", lwd=2)

# Quadratic Data Generation
quadratic_seasonal <- generate_timeseries("quadratic") # With Seasonality
quadratic_non_seasonal <- generate_timeseries("quadratic", seasonality = FALSE, noise = TRUE) # No Seasonality

plot(quadratic_seasonal, main="Time Series Plot of Quadratic Seasonal", xlab="Year", ylab="Value", col="blue", lwd=2)
plot(quadratic_non_seasonal, main="Time Series Plot of Quadratic Non-Seasonal", xlab="Year", ylab="Value", col="blue", lwd=2)


# Exponential Data Generation
exponential_seasonal <- generate_timeseries("exponential") # With Seasonality
exponential_non_seasonal <- generate_timeseries("exponential", seasonality = FALSE, noise = TRUE) # No Seasonality

plot(exponential_seasonal, main="Time Series Plot of Exponential Seasonal", xlab="Year", ylab="Value", col="red", lwd=2)
plot(exponential_non_seasonal, main="Time Series Plot of Exponential Non-Seasonal", xlab="Year", ylab="Value", col="red", lwd=2)


######## 4-7

# 4. Generic Dataset
generic_data <- rnorm(1200, mean = 50, sd = 10)
length(generic_data)


# 5. Matrix-format Dataset
mat_data <- matrix(rnorm(120 * 2, mean = 50, sd = 10), ncol = 2)
colnames(mat_data) <- c("Series1", "Series2")

mat_data_uni <- matrix(rnorm(120 * 2, mean = 50, sd = 10), ncol = 1)
colnames(mat_data_uni) <- c("Series1")
nrow(mat_data)
mat_data


# 6. Data with Anomalies
anomalous_data <- rnorm(120, mean = 50, sd = 10)
anomalous_data[sample(1:120, 5)] <- NA # Introduce some NA values
anomalous_data[sample(1:120, 5)] <- Inf # Introduce some Inf values

# 7. Attributed Time Series
attributed_ts <- ts(rnorm(120, mean = 50, sd = 10), frequency = 12)
attributes(attributed_ts)$src <- "Synthetic Generator"
attributes(attributed_ts)$updated <- Sys.Date()
attributes(attributed_ts)$index <- sample(1:1000, 120)

# 8. Non-Numeric Data
non_numeric <- factor(c("Red", "Blue", "Green", "Red", "Blue"))

# 9. Weather Data
### Weather Data

# Read the CSV file
data <- read.csv("/Users/jay/Downloads/monthly_csv.csv")

# Display the first few rows to inspect the data
head(data)

# Filter the data to use only one source, say "GCAG"
gcag_data <- subset(data, Source == "GCAG")

# Remove the "Source" column as it's now redundant
gcag_data$Source <- NULL

# Convert the "Date" column to Date type
gcag_data$Date <- as.Date(gcag_data$Date, format="%Y-%m-%d")

# Filter data for the years 2000 to 2022
gcag_data <- subset(gcag_data, as.numeric(format(gcag_data$Date, "%Y")) >= 2000 & as.numeric(format(gcag_data$Date, "%Y")) <= 2022)

# Sort data by date
gcag_data <- gcag_data[order(gcag_data$Date), ]

# Extract only the Year and Month from Date for creating a time series
gcag_data$Year <- as.numeric(format(gcag_data$Date, "%Y"))
gcag_data$Month <- as.numeric(format(gcag_data$Date, "%m"))

# Convert data to a time series object
gcag_ts <- ts(gcag_data$Mean, start=c(min(gcag_data$Year), min(gcag_data$Month)), frequency=12)
