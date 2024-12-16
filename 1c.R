# =====================================================================
# Risk Analytics Assignment - Part 1(c)
# Assessing Normality of Negative Log Returns
# =====================================================================

# -----------------------------
# 1. Load Necessary Libraries
# -----------------------------

# Function to install and load packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of required packages
packages <- c("ggplot2", "nortest", "xts", "dplyr")

# Install and load each package
sapply(packages, install_and_load)

# -----------------------------
# 2. Suppress the 'lag()' Warning from xts
# -----------------------------

# Suppress the specific warning related to 'lag()' from 'xts'
options(xts.warn_dplyr_breaks_lag = FALSE)

# -----------------------------
# 3. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
# Assumes 'Crypto_data.csv' is in the working directory
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# -----------------------------
# 4. Define Negative Log Returns Function
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
# 5. Compute Negative Log Returns
# -----------------------------

# Extract Bitcoin and Ethereum prices
bitcoin_prices <- data$Bitcoin
ethereum_prices <- data$Ethereum

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)
ethereum_neg_log_returns <- negative_log_returns(ethereum_prices)

# -----------------------------
# 6. Create Time Series Objects
# -----------------------------

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")  # Adjust the date as needed
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects for negative log returns
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])
ethereum_neg_log_ts <- xts(ethereum_neg_log_returns, order.by = time_sequence[-1])

# -----------------------------
# 7. Create a Directory to Save Plots
# -----------------------------

# Define a directory to save plots
plot_dir <- "plots_part1c"
if(!dir.exists(plot_dir)){
  dir.create(plot_dir)
  cat("Created directory for plots:", plot_dir, "\n")
} else {
  cat("Saving plots to directory:", plot_dir, "\n")
}

# -----------------------------
# 8. Plot Histograms with Overlaid Normal Distribution
# -----------------------------

# Convert xts objects to data frames for ggplot2
neg_log_df <- data.frame(
  Bitcoin_Neg_Log_Returns = coredata(bitcoin_neg_log_ts),
  Ethereum_Neg_Log_Returns = coredata(ethereum_neg_log_ts)
)

# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(filename = file.path(plot_dir, filename), plot = plot, width = 10, height = 6, bg = "white")
}

# Plot Histogram for Bitcoin Negative Log Returns
hist_btc <- ggplot(neg_log_df, aes(x = Bitcoin_Neg_Log_Returns)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.001, fill = "red", alpha = 0.6, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(neg_log_df$Bitcoin_Neg_Log_Returns, na.rm = TRUE),
                                         sd = sd(neg_log_df$Bitcoin_Neg_Log_Returns, na.rm = TRUE)),
                color = "blue", size = 1) +
  ggtitle("Histogram of Bitcoin Negative Log Returns with Normal Curve") +
  xlab("Bitcoin Negative Log Return") +
  ylab("Density") +
  theme_bw()

save_plot(hist_btc, "Histogram_Bitcoin_Negative_Log_Returns.png")
cat("Saved plot: Histogram_Bitcoin_Negative_Log_Returns.png\n")

# Plot Histogram for Ethereum Negative Log Returns
hist_eth <- ggplot(neg_log_df, aes(x = Ethereum_Neg_Log_Returns)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.001, fill = "blue", alpha = 0.6, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(neg_log_df$Ethereum_Neg_Log_Returns, na.rm = TRUE),
                                         sd = sd(neg_log_df$Ethereum_Neg_Log_Returns, na.rm = TRUE)),
                color = "red", size = 1) +
  ggtitle("Histogram of Ethereum Negative Log Returns with Normal Curve") +
  xlab("Ethereum Negative Log Return") +
  ylab("Density") +
  theme_bw()

save_plot(hist_eth, "Histogram_Ethereum_Negative_Log_Returns.png")
cat("Saved plot: Histogram_Ethereum_Negative_Log_Returns.png\n")

# -----------------------------
# 9. Plot QQ-Plots
# -----------------------------

# Function to create and save QQ-plot
create_qqplot <- function(data, crypto_name) {
  qq_plot <- ggplot(data, aes(sample = data[[crypto_name]])) +
    stat_qq(color = "darkgreen") +
    stat_qq_line(color = "red") +
    ggtitle(paste("QQ-Plot of", crypto_name, "Negative Log Returns")) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme_bw()

  filename <- paste0("QQPlot_", crypto_name, "_Negative_Log_Returns.png")
  save_plot(qq_plot, filename)
  cat("Saved plot:", filename, "\n")
}

# Create QQ-Plot for Bitcoin Negative Log Returns
create_qqplot(neg_log_df, "Bitcoin_Neg_Log_Returns")

# Create QQ-Plot for Ethereum Negative Log Returns
create_qqplot(neg_log_df, "Ethereum_Neg_Log_Returns")

# -----------------------------
# 10. Perform Anderson-Darling Test for Normality
# -----------------------------

# Function to perform Anderson-Darling test and save results
perform_ad_test <- function(time_series, crypto_name) {
  # Perform Anderson-Darling Test
  ad_result <- ad.test(coredata(time_series))

  # Create a text file to save the test results
  ad_output_file <- file.path(plot_dir, paste0("Anderson_Darling_Test_", crypto_name, ".txt"))
  sink(ad_output_file)
  cat(paste0("\nAnderson-Darling Test for ", crypto_name, " Negative Log Returns:\n"))
  print(ad_result)

  # Interpretation based on p-value
  if(ad_result$p.value < 0.05){
    cat("Result: The ", crypto_name, " negative log returns are NOT normally distributed (reject the null hypothesis of normality).\n", sep = "")
  } else {
    cat("Result: The ", crypto_name, " negative log returns are normally distributed (fail to reject the null hypothesis of normality).\n", sep = "")
  }
  sink()

  # Additionally, print to console
  cat("\nAnderson-Darling Test for ", crypto_name, " Negative Log Returns:\n", sep = "")
  print(ad_result)
  if(ad_result$p.value < 0.05){
    cat("Result: The ", crypto_name, " negative log returns are NOT normally distributed (reject the null hypothesis of normality).\n", sep = "")
  } else {
    cat("Result: The ", crypto_name, " negative log returns are normally distributed (fail to reject the null hypothesis of normality).\n", sep = "")
  }
}

# Perform Anderson-Darling Test for Bitcoin Negative Log Returns
perform_ad_test(bitcoin_neg_log_ts, "Bitcoin_Neg_Log_Returns")

# Perform Anderson-Darling Test for Ethereum Negative Log Returns
perform_ad_test(ethereum_neg_log_ts, "Ethereum_Neg_Log_Returns")

# =====================================================================
# End of Part 1(c) Script
# =====================================================================
