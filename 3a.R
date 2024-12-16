# =====================================================================
# Risk Analytics Assignment - Part 3(a)
# Correlation Analysis between Bitcoin and Ethereum Negative Log Returns
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
packages <- c("ggplot2", "xts", "dplyr", "gridExtra")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that required columns exist
required_columns <- c("Bitcoin", "Ethereum")
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
# 4. Compute Negative Log Returns for Bitcoin and Ethereum
# -----------------------------

# Extract Bitcoin and Ethereum prices
bitcoin_prices <- data$Bitcoin
ethereum_prices <- data$Ethereum

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)
ethereum_neg_log_returns <- negative_log_returns(ethereum_prices)

# -----------------------------
# 5. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for negative log returns
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])
ethereum_neg_log_ts <- xts(ethereum_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 6. Create a Directory to Save Results
# -----------------------------

# Define a directory to save plots and results
result_dir <- "plots_part3a"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 7. Perform Correlation Test
# -----------------------------

# Ensure both series have the same length
min_length <- min(length(bitcoin_neg_log_ts), length(ethereum_neg_log_ts))
bitcoin_neg_log_ts <- bitcoin_neg_log_ts[1:min_length]
ethereum_neg_log_ts <- ethereum_neg_log_ts[1:min_length]

# Convert xts objects to numeric vectors
btc_returns <- as.numeric(bitcoin_neg_log_ts)
eth_returns <- as.numeric(ethereum_neg_log_ts)

# Perform Pearson correlation test
cor_test_result <- cor.test(btc_returns, eth_returns, method = "pearson")

# Print the results to console
print(cor_test_result)

# Save the correlation test results to a text file
sink(file = file.path(result_dir, "Correlation_Test_Results.txt"))
cat("Correlation Test between Bitcoin and Ethereum Negative Log Returns:\n\n")
print(cor_test_result)
sink()
cat("Saved Correlation Test Results to 'Correlation_Test_Results.txt'.\n")

# -----------------------------
# 8. Visualize the Relationship
# -----------------------------

# Create a data frame for plotting
plot_df <- data.frame(
  Bitcoin_NegLogReturns = btc_returns,
  Ethereum_NegLogReturns = eth_returns
)

# Generate scatter plot with white background
scatter_plot <- ggplot(plot_df, aes(x = Bitcoin_NegLogReturns, y = Ethereum_NegLogReturns)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter Plot of Bitcoin vs Ethereum Negative Log Returns") +
  xlab("Bitcoin Negative Log Returns") +
  ylab("Ethereum Negative Log Returns") +
  theme_bw()  # This ensures a white background

# Save the scatter plot
ggsave(filename = file.path(result_dir, "Scatter_Plot_BTC_ETH_NegLogReturns.png"),
       plot = scatter_plot, width = 8, height = 6, dpi = 300)
cat("Saved Scatter Plot to 'Scatter_Plot_BTC_ETH_NegLogReturns.png'.\n")

# =====================================================================
# End of Part 3(a) Script
# =====================================================================
