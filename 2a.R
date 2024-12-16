# =====================================================================
# Risk Analytics Assignment - Part 2(a)
# Financial Time Series, Heteroscedasticity, and the Random Walk Hypothesis
# =====================================================================

# -----------------------------
# 1. Load Necessary Libraries
# -----------------------------

# Function to install and load required packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    if(!require(package_name, character.only = TRUE)){
      stop(paste("Package", package_name, "could not be loaded"))
    }
  }
}

# List of required packages
packages <- c("ggplot2", "xts", "zoo", "dplyr", "gridExtra")

# Install and load each package, suppressing output
invisible(sapply(packages, install_and_load))

# -----------------------------
# 2. Read the Crypto_data.csv File
# -----------------------------

# Define the path to the CSV file
data_file <- "Crypto_data.csv"

# Check if the file exists
if(!file.exists(data_file)){
  stop(paste("Data file", data_file, "not found in the working directory."))
}

# Read the CSV file into a data frame
crypto_data <- read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)

# Verify that the 'Bitcoin' column exists
if(!"Bitcoin" %in% names(crypto_data)){
  stop("The 'Bitcoin' column is missing in 'Crypto_data.csv'.")
}

# -----------------------------
# 3. Compute Negative Log Returns
# -----------------------------

# Function to compute negative log returns
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

# Extract Bitcoin prices
bitcoin_prices <- crypto_data$Bitcoin

# Compute negative log returns
bitcoin_neg_log_returns <- negative_log_returns(bitcoin_prices)

# -----------------------------
# 4. Create Time Series Object
# -----------------------------

# Assuming data is recorded every minute for 24 hours (1440 minutes)
# Create a sequence of POSIXct timestamps
start_time <- as.POSIXct("2024-12-01 00:00:00")  # Example start date
time_sequence <- seq(from = start_time, by = "min", length.out = length(bitcoin_prices))

# Create time series objects
bitcoin_neg_log_ts <- xts(bitcoin_neg_log_returns, order.by = time_sequence[-1])
bitcoin_raw_ts <- xts(bitcoin_prices, order.by = time_sequence)

# -----------------------------
# 5. Create a Directory to Save Plots
# -----------------------------

# Define a directory to save plots
plot_dir <- "plots_part2a"

# Create the directory if it doesn't exist
if(!dir.exists(plot_dir)){
  dir.create(plot_dir)
  cat("Created directory for plots:", plot_dir, "\n")
} else {
  cat("Saving plots to directory:", plot_dir, "\n")
}

# -----------------------------
# 6. Prepare Data for ACF Plotting
# -----------------------------

# Extract core data and remove any NAs
bitcoin_neg_log_data <- as.numeric(na.omit(coredata(bitcoin_neg_log_ts)))
bitcoin_raw_data <- as.numeric(na.omit(coredata(bitcoin_raw_ts)))

# Debugging: Print lengths and first few elements
cat("Length of Raw Bitcoin Prices:", length(bitcoin_raw_data), "\n")
cat("First few Raw Bitcoin Prices:", head(bitcoin_raw_data), "\n\n")

cat("Length of Bitcoin Negative Log Returns:", length(bitcoin_neg_log_data), "\n")
cat("First few Bitcoin Negative Log Returns:", head(bitcoin_neg_log_data), "\n\n")

# -----------------------------
# 7. Plot the ACF of Raw Series and Negative Log Returns
# -----------------------------

# Define a function to create ACF plots using base R and ggplot2
create_acf_plot <- function(data_vector, title) {
  # Compute ACF
  acf_result <- acf(data_vector, plot = FALSE)

  # Convert ACF to data frame for ggplot2
  acf_df <- with(acf_result, data.frame(lag, acf))

  # Create ACF plot using ggplot2
  p <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(1.96/sqrt(length(data_vector)), -1.96/sqrt(length(data_vector))),
               color = "red", linetype = "dashed") +
    labs(title = title, x = "Lag", y = "Autocorrelation") +
    theme_minimal()

  return(p)
}

# Create ACF plots
acf_raw_plot <- create_acf_plot(bitcoin_raw_data, "ACF of Raw Bitcoin Prices")
acf_neg_log_plot <- create_acf_plot(bitcoin_neg_log_data, "ACF of Bitcoin Negative Log Returns")

# Arrange both plots side by side
combined_acf_plot <- gridExtra::grid.arrange(acf_raw_plot, acf_neg_log_plot, ncol = 2)

# -----------------------------
# 8. Save the Combined ACF Plot
# -----------------------------

# Define the filename for the combined ACF plot
acf_plot_filename <- "ACF_Plot_Raw_vs_NegLogReturns_Bitcoin.png"

# Save the plot
ggsave(filename = file.path(plot_dir, acf_plot_filename), plot = combined_acf_plot,
       width = 16, height = 8, dpi = 300, bg = "white")

cat("Saved ACF plot as:", acf_plot_filename, "\n")

# =====================================================================
# End of Part 2(a) Script
# =====================================================================
