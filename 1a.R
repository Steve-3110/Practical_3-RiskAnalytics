# =====================================================================
# Risk Analytics Project - Part 1(a)
# Assessing Stationarity of Bitcoin Prices
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
packages <- c("tseries", "ggplot2", "xts")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Create a Directory to Save Results
# -----------------------------

# Define a directory to save plots and results
result_dir <- "plots_part1a"
if(!dir.exists(result_dir)){
  dir.create(result_dir)
  cat("Created directory for results:", result_dir, "\n")
} else {
  cat("Saving results to directory:", result_dir, "\n")
}

# -----------------------------
# 3. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that the 'Bitcoin' column exists
if(!"Bitcoin" %in% names(data)){
  stop("The 'Bitcoin' column is missing in 'Crypto_data.csv'. Please check the file.")
}

# -----------------------------
# 4. Extract Bitcoin Prices and Create Time Series Object
# -----------------------------

# Extract Bitcoin prices
bitcoin_prices <- data$Bitcoin

# Check for non-numeric values
if(!is.numeric(bitcoin_prices)){
  stop("Bitcoin prices must be numeric.")
}

# Check for non-positive prices to avoid log issues (if needed later)
if(any(bitcoin_prices <= 0, na.rm = TRUE)) {
  stop("Bitcoin prices must be positive.")
}

# Convert to a time series object
# Assuming the data is recorded every minute for 24 hours (1440 minutes)
start_time <- as.POSIXct("2024-10-22 00:00:00")  # Adjust the start date as needed
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))
bitcoin_ts <- xts(bitcoin_prices, order.by = time_sequence)

# -----------------------------
# 5. Plot the Raw Bitcoin Prices
# -----------------------------

# 5.1: Base R Plot
png(filename = file.path(result_dir, "Bitcoin_Prices_Time_Series_BaseR.png"),
    width = 1200, height = 800, res = 150)
plot(bitcoin_ts, main = "Bitcoin Prices Over Time (Base R)",
     ylab = "Price (USD)", xlab = "Time", col = "blue")
dev.off()
cat("Saved Base R Time Series Plot to 'Bitcoin_Prices_Time_Series_BaseR.png'.\n")

# 5.2: ggplot2 Plot
# Convert xts to data frame for ggplot2
bitcoin_df <- data.frame(
  Time = index(bitcoin_ts),
  Price = coredata(bitcoin_ts)
)

# Generate ggplot2 time series plot with theme_bw
ggplot_plot <- ggplot(bitcoin_df, aes(x = Time, y = Price)) +
  geom_line(color = "darkgreen") +
  ggtitle("Bitcoin Prices Over Time") +
  xlab("Time") +
  ylab("Price (USD)") +
  theme_bw()

# Save the ggplot2 plot
ggsave(filename = file.path(result_dir, "Bitcoin_Prices_Time_Series_GGPlot2.png"),
       plot = ggplot_plot, width = 10, height = 6, dpi = 300)
cat("Saved ggplot2 Time Series Plot to 'Bitcoin_Prices_Time_Series_GGPlot2.png'.\n")

# -----------------------------
# 6. Plot the Autocorrelation Function (ACF)
# -----------------------------

# 6.1: Base R ACF Plot with Increased Lag Max
png(filename = file.path(result_dir, "Bitcoin_Prices_ACF_BaseR.png"),
    width = 1200, height = 800, res = 150)
acf(bitcoin_ts, main = "Autocorrelation Function (ACF) of Bitcoin Prices (Base R)", lag.max = 100)
dev.off()
cat("Saved Base R ACF Plot to 'Bitcoin_Prices_ACF_BaseR.png'.\n")

# 6.2: ggplot2 ACF Plot (Corrected with Theme_bw)
# Compute ACF values with higher lag.max
acf_values <- acf(bitcoin_ts, plot = FALSE, lag.max = 100)

# Flatten the ACF and Lag matrices to vectors
acf_df <- data.frame(
  Lag = as.numeric(acf_values$lag),
  ACF = as.numeric(acf_values$acf)
)

# Remove lag 0 for better visualization (optional)
acf_df <- acf_df[acf_df$Lag != 0, ]

# Generate ggplot2 ACF plot with theme_bw
ggplot_acf <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(2/sqrt(length(bitcoin_ts)), -2/sqrt(length(bitcoin_ts))),
             color = "red", linetype = "dashed") +
  ggtitle("Autocorrelation Function (ACF) of Bitcoin Prices") +
  xlab("Lag (minutes)") +
  ylab("ACF") +
  theme_bw()

# Save the ggplot2 ACF plot
ggsave(filename = file.path(result_dir, "Bitcoin_Prices_ACF_GGPlot2.png"),
       plot = ggplot_acf, width = 10, height = 6, dpi = 300)
cat("Saved ggplot2 ACF Plot to 'Bitcoin_Prices_ACF_GGPlot2.png'.\n")

# -----------------------------
# 7. Perform Augmented Dickey-Fuller (ADF) Test
# -----------------------------

# Perform ADF test
# Null hypothesis: The time series has a unit root (is non-stationary)
# Alternative hypothesis: The time series is stationary
adf_result <- adf.test(bitcoin_ts, alternative = "stationary")

# Print ADF test results to console
print(adf_result)

# -----------------------------
# 8. Save ADF Test Results and Interpretation
# -----------------------------

# Create a text file to save the ADF test results and interpretation
sink(file = file.path(result_dir, "ADF_Test_Result_and_Interpretation.txt"))
cat("Augmented Dickey-Fuller (ADF) Test Results for Bitcoin Prices\n")
cat("-----------------------------------------------------------\n\n")
print(adf_result)
cat("\nInterpretation:\n")
if(adf_result$p.value < 0.05){
  cat("The p-value is less than 0.05. We reject the null hypothesis.\n")
  cat("Conclusion: The Bitcoin price series is stationary.\n")
} else {
  cat("The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis.\n")
  cat("Conclusion: The Bitcoin price series is non-stationary.\n")
}
sink()
cat("Saved ADF Test Results and Interpretation to 'ADF_Test_Result_and_Interpretation.txt'.\n")
