# =====================================================================
# Risk Analytics Assignment - Part 2(b)
# Ljung-Box Test for Serial Dependence in Raw Series and Negative Log Returns
# =====================================================================

# -----------------------------
# 1. Load Necessary Libraries
# -----------------------------

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    if(!require(package_name, character.only = TRUE)){
      stop(paste("Package", package_name, "could not be loaded"))
    }
  }
}

# List of required packages
packages <- c("ggplot2", "xts", "dplyr", "gridExtra", "nortest", "forecast")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that required columns exist
required_columns <- c("Bitcoin")
missing_columns <- setdiff(required_columns, names(data))
if(length(missing_columns) > 0){
  stop(paste("The following required columns are missing in 'Crypto_data.csv':",
             paste(missing_columns, collapse = ", ")))
}

# -----------------------------
# 3. Define Negative Log Returns Function
# -----------------------------

negative_log_returns <- function(prices) {
  # Ensure the input is numeric
  prices <- as.numeric(prices)

  # Check for non-positive prices to avoid log issues
  if(any(prices <= 0, na.rm = TRUE)) {
    stop("Prices must be positive to compute logarithms.")
  }

  # Compute log returns: log(P_t) - log(P_{t-1})
  log_returns <- diff(log(prices))

  # Compute negative log returns
  neg_log_returns <- -log_returns

  return(neg_log_returns)
}

# -----------------------------
# 4. Compute Negative Log Returns
# -----------------------------

# Extract Bitcoin prices
bitcoin_prices <- data$Bitcoin

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)

# -----------------------------
# 5. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")  # Adjust the date as needed
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for raw prices and negative log returns
bitcoin_raw_ts <- xts(bitcoin_prices, order.by = time_sequence)
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 6. Create a Directory to Save Results
# -----------------------------

# Define a directory to save results
result_dir <- "plots_part2b"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 7. Perform Ljung-Box Tests
# -----------------------------

# Function to perform Ljung-Box test and print results
perform_ljung_box <- function(ts_data, series_name, max_lag = 20) {
  # Perform Ljung-Box test
  ljung_box_result <- Box.test(ts_data, lag = max_lag, type = "Ljung-Box")

  # Print the results
  cat("\nLjung-Box Test for", series_name, ":\n")
  print(ljung_box_result)

  # Interpretation
  if(ljung_box_result$p.value < 0.05){
    cat("Conclusion: Reject the null hypothesis. There is evidence of serial dependence in", series_name, ".\n")
  } else {
    cat("Conclusion: Do not reject the null hypothesis. No evidence of serial dependence in", series_name, ".\n")
  }
}

# Perform Ljung-Box test on Raw Bitcoin Prices
perform_ljung_box(bitcoin_raw_ts, "Raw Bitcoin Prices")

# Perform Ljung-Box test on Bitcoin Negative Log Returns
perform_ljung_box(bitcoin_neg_log_ts, "Bitcoin Negative Log Returns")

# -----------------------------
# 8. Save Test Results to a Text File (Optional)
# -----------------------------

# Redirect console output to a text file
sink(file = file.path(result_dir, "LjungBox_Test_Results.txt"))

# Perform Ljung-Box test on Raw Bitcoin Prices
perform_ljung_box(bitcoin_raw_ts, "Raw Bitcoin Prices")

# Perform Ljung-Box test on Bitcoin Negative Log Returns
perform_ljung_box(bitcoin_neg_log_ts, "Bitcoin Negative Log Returns")

# Stop redirecting output
sink()

cat("Ljung-Box test results saved to 'LjungBox_Test_Results.txt' in", result_dir, "directory.\n")

# =====================================================================
# End of Part 2(b) Script
# =====================================================================
