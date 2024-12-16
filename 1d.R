# =====================================================================
# Risk Analytics Project - Part 1(d)
# Fit a t-distribution to the negative log returns
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
packages <- c("tseries", "ggplot2", "MASS")

# -----------------------------
# 2. Read the Crypto_data.csv File
# -----------------------------

# Read the CSV file into a data frame
data <- read.csv("Crypto_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Verify that the 'Bitcoin' column exists
if(!"Bitcoin" %in% names(data)){
  stop("The 'Bitcoin' column is missing in 'Crypto_data.csv'. Please check the file.")
}

# Example of computing negative log returns if needed:
# d_crypto$neg_log_returns_bitcoin <- -diff(log(d_crypto$Bitcoin))

# Check for non-finite values in Bitcoin returns (assuming it's called neg_log_returns_bitcoin)
bitcoin_returns <- na.omit(d_crypto$neg_log_returns_bitcoin)

# Remove any remaining non-finite values
bitcoin_returns <- bitcoin_returns[is.finite(bitcoin_returns)]

# Ensure the length of bitcoin_returns is sufficient for distribution fitting and plotting
if (length(bitcoin_returns) <= 1) {
  stop("Not enough data in bitcoin_returns for further analysis.")
}

# -----------------------------
# 3. Fit a t-distribution using the ghyp package
# -----------------------------

fit_t <- fit.tuv(bitcoin_returns)

# Extract degrees of freedom (df) for the t-distribution
df_t <- fit_t@lambda  # Lambda corresponds to degrees of freedom (df)

# -----------------------------
# 4. Fit a normal distribution using MASS
# -----------------------------

fit_norm <- fitdistr(bitcoin_returns, "normal")

# Extract the parameters of the fitted normal distribution
mean_norm <- fit_norm$estimate["mean"]
sd_norm <- fit_norm$estimate["sd"]

# -----------------------------
#  5. Plot histogram of Bitcoin returns and overlay t-distribution and normal distribution densities
# -----------------------------
ggplot() +
  geom_histogram(aes(x = bitcoin_returns, y = ..density..), 
                 bins = 50, fill = "lightblue", alpha = 0.7, 
                 color = "black", boundary = 0) +
  stat_function(fun = function(x) dghyp(x, fit_t), 
                color = "red", size = 1.2, 
                linetype = "solid", 
                aes(label = "t-distribution (ghyp)")) +  # t-distribution density
  stat_function(fun = dnorm, args = list(mean = mean_norm, sd = sd_norm), 
                color = "blue", size = 1.2, 
                linetype = "dashed", 
                aes(label = "Normal distribution")) +  # Normal distribution density
  labs(title = "Empirical Bitcoin Returns with Fitted Distributions",
       x = "Bitcoin Returns", 
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")
