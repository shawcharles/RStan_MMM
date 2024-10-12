##################### Section: Environment Setup #####################
# Suppress warnings for cleaner output
options(warn=-1)

# Install and load required packages
if (!require(rstan)) install.packages("rstan")
if (!require(jsonlite)) install.packages("jsonlite")

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(corrplot)
library(GGally)
library(rstan)
library(jsonlite)
library(treemapify)
library(dplyr)

# Clear the console output
cat("\014")  

# Set the working directory for the project files
setwd("P:/charles.shaw/_RSTAN MMM Model")

# Iterations and chains

n_iter <- 1000 #2000
n_chains <- 3  #4

##################### Section: Data Loading #####################
# Load data containing four years (209 weeks) of sales, media impressions, and media spending data
df <- read.csv('data_demo.csv')

##################### Section: Data Preparation #####################
# Define media impression variables
mdip_cols <- grep("mdip_", names(df), value = TRUE)
# Define media spending variables
mdsp_cols <- grep("mdsp_", names(df), value = TRUE)

# Define control variables for the model
me_cols <- grep("me_", names(df), value = TRUE)  # macro economics variables
st_cols <- c('st_ct')                            # store count variables
mrkdn_cols <- grep("mrkdn_", names(df), value = TRUE)  # markdown/discount variables
hldy_cols <- grep("hldy_", names(df), value = TRUE)    # holiday variables
seas_cols <- grep("seas_", names(df), value = TRUE)    # seasonality variables
base_vars <- c(me_cols, st_cols, mrkdn_cols, hldy_cols, seas_cols)

# Define sales variables
sales_cols <- c('sales')

# Ensure the date variable is in the correct format
df$date <- as.Date(df$date)

##################### Section: Exploratory Data Analysis #####################
# Display head of specific columns to check data
head(df[c('date', mdip_cols, 'sales')])

# Create a correlation plot and save to file
png("correlation_plot.png", width = 800, height = 600)
corrplot(cor(df[c(mdip_cols, 'sales')]), 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         title = "Correlation Plot of Media Impressions and KPI")
dev.off()
print(paste("Correlation plot saved as:", file.path(getwd(), "correlation_plot.png")))

# Generate pairwise plots for media impressions and sales
ggpairs(df[c(mdip_cols, 'sales')])

##################### Section: Adstock Transformation #####################
# Function to apply adstock effect to a series
apply_adstock <- function(x, L, P, D) {
  x <- c(rep(0, L-1), x)
  weights <- numeric(L)
  for (l in 1:L) {
    weight <- D^((l-P)^2)
    weights[L-l+1] <- weight
  }
  adstocked_x <- numeric(length(x) - L + 1)
  for (i in L:length(x)) {
    x_array <- x[(i-L+1):i]
    xi <- sum(x_array * weights) / sum(weights)
    adstocked_x[i-L+1] <- xi
  }
  return(adstocked_x)
}

# Function to transform all specified media columns in a dataframe using adstock parameters
adstock_transform <- function(df, md_cols, adstock_params) {
  md_df <- data.frame(matrix(ncol = length(md_cols), nrow = nrow(df)))
  colnames(md_df) <- md_cols
  
  for (md_col in md_cols) {
    md <- tail(strsplit(md_col, "_")[[1]], 1)
    L <- adstock_params[[md]]$L
    P <- adstock_params[[md]]$P
    D <- adstock_params[[md]]$D
    xa <- apply_adstock(df[[md_col]], L, P, D)
    md_df[[md_col]] <- xa
  }
  return(md_df)
}

##################### Section: Plotting Adstock Effects #####################
# Function to plot adstock effects with varying decay and length
plot_adstock <- function(df, column, psets, title) {
  xm <- df[[column]]
  plot_data <- data.frame(x = 1:52, original = tail(xm, 52))
  
  for (i in seq_along(psets)) {
    p <- psets[[i]]
    L <- p[1]
    P <- p[2]
    D <- p[3]
    xm_adstocked <- tail(apply_adstock(xm, L, P, D), 52)
    plot_data[[paste("L", L, "P", P, "D", D, sep = "_")]] <- xm_adstocked
  }
  
  plot_data_long <- tidyr::pivot_longer(plot_data, -x, names_to = "variable", values_to = "value")
  
  ggplot(plot_data_long, aes(x = x, y = value, color = variable)) +
    geom_line() +
    labs(title = title, x = "Week", y = "Value") +
    theme_minimal()
}

# Plot adstock with varying decay
psets_decay <- list(c(8, 1, 0.1), c(8, 1, 0.9))
plot_adstock(df, "mdip_vidtr", psets_decay, "Adstock Parameter: Decay")

# Plot adstock with varying length
psets_length <- list(c(4, 1, 0.9), c(12, 1, 0.9))
plot_adstock(df, "mdip_vidtr", psets_length, "Adstock Parameter: Length")


# 1.2 Diminishing Return
hill_transform <- function(x, ec, slope) {
  return(1 / (1 + (x / ec)^(-slope)))
}

# Plot hill function with varying K and S

plot_hill_function <- function(psets) {
  xm <- seq(0, 2, by = 0.05)
  plot_data <- data.frame(x = xm)
  
  for (i in seq_along(psets)) {
    p <- psets[[i]]
    ec <- p[1]
    slope <- p[2]
    plot_data[[paste("K", ec, "S", slope, sep = "_")]] <- hill_transform(xm, ec, slope)
  }
  
  plot_data_long <- tidyr::pivot_longer(plot_data, -x, names_to = "variable", values_to = "value")
  
  ggplot(plot_data_long, aes(x = x, y = value, color = variable)) +
    geom_line() +
    labs(title = "Hill Function", x = "x", y = "y") +
    theme_minimal()
}

psets_hill <- list(c(0.5, 0.5), c(0.5, 1.0), c(0.95, 1.0), c(0.95, 3.0))
plot_hill_function(psets_hill)


##################### Section: Model Implementation Overview #####################
# Description of the overall modelling approach involving three distinct models:
# 1. Control Model
# 2. Marketing Mix Model
# 3. Diminishing Return Model

##################### Section: Control Model / Base Sales Model #####################
# This section includes the implementation of the Control Model which acts as the baseline for sales prediction.

# Define helper functions to support model calculations:

# Function to calculate Mean Absolute Percentage Error (MAPE)
mean_absolute_percentage_error <- function(y_true, y_pred) {
  return(mean(abs((y_true - y_pred) / y_true)) * 100)
}

# Function to apply mean centering to a numeric variable
apply_mean_center <- function(x) {
  mu <- mean(x)
  xm <- x / mu
  return(list(xm = xm, mu = mu))
}

# Function to transform dataframe columns by mean centering
mean_center_transform <- function(df, cols) {
  cat("Starting mean_center_transform\n")
  cat("Input df structure:\n")
  print(str(df))
  cat("Columns to process:", paste(cols, collapse = ", "), "\n")
  cat("Number of columns:", length(cols), "\n")
  cat("Number of rows in df:", nrow(df), "\n")
  
  if (!is.data.frame(df)) {
    stop("Input 'df' is not a data frame")
  }
  
  if (length(cols) == 0) {
    stop("No columns specified")
  }
  
  if (nrow(df) == 0) {
    stop("Input data frame has no rows")
  }
  
  df_new <- data.frame(matrix(ncol = length(cols), nrow = nrow(df)))
  colnames(df_new) <- cols
  sc <- list()
  
  for (col in cols) {
    cat("Processing column:", col, "\n")
    if (!(col %in% names(df))) {
      stop("Column '", col, "' not found in the input data frame")
    }
    x <- df[[col]]
    if (!is.numeric(x)) {
      stop("Column '", col, "' is not numeric")
    }
    result <- apply_mean_center(x)
    df_new[[col]] <- result$xm
    sc[[col]] <- result$mu
  }
  
  cat("Finished mean_center_transform\n")
  return(list(df_new = df_new, sc = sc))
}

# Function to transform columns using logarithm of (1+x) after mean centering
mean_log1p_transform <- function(df, cols) {
  df_new <- data.frame(matrix(ncol = length(cols), nrow = nrow(df)))
  colnames(df_new) <- cols
  sc <- list()
  
  for (col in cols) {
    x <- df[[col]]
    result <- apply_mean_center(x)
    sc[[col]] <- result$mu
    df_new[[col]] <- log1p(result$xm)
  }
  
  return(list(df_new = df_new, sc = sc))
}

# Save data to JSON format
save_json <- function(data, file_name) {
  write_json(data, file_name)
}

# Load data from JSON format
load_json <- function(file_name) {
  return(read_json(file_name))
}

##################### Section: Data Transformation for Control Model #####################
# Apply mean centering to sales and control variables
result_ctrl <- mean_center_transform(df, c('sales', me_cols, st_cols, mrkdn_cols))
df_ctrl <- result_ctrl$df_new
sc_ctrl <- result_ctrl$sc

# Combine mean-centered data with holiday and seasonality variables for further analysis
df_ctrl <- cbind(df_ctrl, df[c(hldy_cols, seas_cols)])

# Define variables influencing sales positively
pos_vars <- setdiff(base_vars, seas_cols)
X1 <- as.matrix(df_ctrl[, pos_vars])

# Define variables that may influence sales positively or negatively (seasonality factors)
pn_vars <- seas_cols
X2 <- as.matrix(df_ctrl[, pn_vars])

##################### Section: Control Model Data Preparation #####################
# Prepare data list for input to the statistical model
ctrl_data <- list(
  N = nrow(df_ctrl),
  K1 = length(pos_vars),
  K2 = length(pn_vars),
  X1 = X1,
  X2 = X2,
  y = df_ctrl$sales,
  max_intercept = min(df_ctrl$sales)
)


##################### Section: Stan Model for Control Model #####################
# Define the Stan model for the Control Model
ctrl_code1 <- "
data {
  int<lower=0> N; // number of observations
  int<lower=0> K1; // number of positive predictors
  int<lower=0> K2; // number of positive/negative predictors
  real max_intercept; // maximum value for the intercept to prevent unrealistic high predictions
  matrix[N, K1] X1; // matrix of variables with positive impacts
  matrix[N, K2] X2; // matrix of variables with possible negative impacts
  vector[N] y; // vector of observed sales
}
parameters {
  vector<lower=0>[K1] beta1; // coefficients for positive impact variables
  vector[K2] beta2; // coefficients for variables with potential negative impacts
  real<lower=0, upper=max_intercept> alpha; // intercept
  real<lower=0> noise_var; // noise variance
}
model {
  beta1 ~ normal(0, 1); // prior for beta1
  beta2 ~ normal(0, 1); // prior for beta2
  noise_var ~ inv_gamma(0.05, 0.05 * 0.01); // prior for noise variance
  y ~ normal(X1 * beta1 + X2 * beta2 + alpha, sqrt(noise_var)); // likelihood
}
"

# Compile the Stan model
sm1 <- stan_model(model_code = ctrl_code1)

# Run the MCMC sampling
fit1 <- sampling(sm1, data = ctrl_data, iter = n_iter, chains = n_chains)

# Extract the results
fit1_result <- rstan::extract(fit1)

##################### Section: Control Model Results Extraction and Prediction #####################
# Define a function to extract model parameters and optionally return them as a list
extract_ctrl_model <- function(fit_result, pos_vars, pn_vars, extract_param_list = FALSE) {
  ctrl_model <- list(
    pos_vars = pos_vars,
    pn_vars = pn_vars,
    beta1 = colMeans(fit_result$beta1),
    beta2 = colMeans(fit_result$beta2),
    alpha = mean(fit_result$alpha)
  )
  
  if (extract_param_list) {
    ctrl_model$beta1_list <- fit_result$beta1
    ctrl_model$beta2_list <- fit_result$beta2
    ctrl_model$alpha_list <- fit_result$alpha
  }
  
  return(ctrl_model)
}

# Define a function to predict base sales using the control model
ctrl_model_predict <- function(ctrl_model, df) {
  X1 <- as.matrix(df[, ctrl_model$pos_vars])
  X2 <- as.matrix(df[, ctrl_model$pn_vars])
  beta1 <- matrix(ctrl_model$beta1, ncol = 1)
  beta2 <- matrix(ctrl_model$beta2, ncol = 1)
  alpha <- ctrl_model$alpha
  
  y_pred <- X1 %*% beta1 + X2 %*% beta2 + alpha
  return(as.vector(y_pred))
}

# Extract parameters and predict base sales
base_sales_model <- extract_ctrl_model(fit1_result, pos_vars, pn_vars)
base_sales <- ctrl_model_predict(base_sales_model, df_ctrl)
df$base_sales <- base_sales * sc_ctrl$sales

# Evaluate the control model
cat("MAPE:", mean_absolute_percentage_error(df$sales, df$base_sales), "\n")

##################### Section: Model Saving and Loading #####################
# Uncomment these lines to save/load results for later use
#write.csv(df$base_sales, "base_sales_pred.csv", row.names = FALSE)
#write_json(base_sales_model, "ctrl_model.json")
#df$base_sales <- read.csv("base_sales_pred.csv", header = FALSE)[[1]]

##################### Section: Marketing Mix Model #####################
# Perform transformations for the Marketing Mix Model
result_mmm <- mean_log1p_transform(df, c('sales', 'base_sales'))
df_mmm <- result_mmm$df_new
sc_mmm <- result_mmm$sc

# Calculate mean of media impressions
mu_mdip <- colMeans(df[mdip_cols])
max_lag <- 8
num_media <- length(mdip_cols)

# Prepare media data with padding for lag effects
X_media <- rbind(matrix(0, nrow = max_lag - 1, ncol = num_media), as.matrix(df[mdip_cols]))
X_ctrl <- matrix(df_mmm$base_sales, ncol = 1)

# Prepare data for the Stan model
model_data2 <- list(
  N = nrow(df),
  max_lag = max_lag,
  num_media = num_media,
  X_media = X_media,
  mu_mdip = mu_mdip,
  num_ctrl = ncol(X_ctrl),
  X_ctrl = X_ctrl,
  y = df_mmm$sales
)

##################### Section: Stan Model Code for Marketing Mix Model #####################
# Define Stan model code for the Marketing Mix Model
model_code2 <- "
functions {
  // the adstock transformation with a vector of weights
  real Adstock(vector t, row_vector weights) {
    return dot_product(t, weights) / sum(weights);
  }
}
data {
  // the total number of observations
  int<lower=1> N;
  // the vector of sales
  real y[N];
  // the maximum duration of lag effect, in weeks
  int<lower=1> max_lag;
  // the number of media channels
  int<lower=1> num_media;
  // matrix of media variables
  matrix[N+max_lag-1, num_media] X_media;
  // vector of media variables' mean
  real mu_mdip[num_media];
  // the number of other control variables
  int<lower=1> num_ctrl;
  // a matrix of control variables
  matrix[N, num_ctrl] X_ctrl;
}
parameters {
  // residual variance
  real<lower=0> noise_var;
  // the intercept
  real tau;
  // the coefficients for media variables and base sales
  vector<lower=0>[num_media+num_ctrl] beta;
  // the decay and peak parameter for the adstock transformation of
  // each media
  vector<lower=0,upper=1>[num_media] decay;
  vector<lower=0,upper=ceil(max_lag/2)>[num_media] peak;
}
transformed parameters {
  // the cumulative media effect after adstock
  real cum_effect;
  // matrix of media variables after adstock
  matrix[N, num_media] X_media_adstocked;
  // matrix of all predictors
  matrix[N, num_media+num_ctrl] X;
  
  // adstock, mean-center, log1p transformation
  row_vector[max_lag] lag_weights;
  for (nn in 1:N) {
    for (media in 1 : num_media) {
      for (lag in 1 : max_lag) {
        lag_weights[max_lag-lag+1] <- pow(decay[media], (lag - 1 - peak[media]) ^ 2);
      }
     cum_effect <- Adstock(sub_col(X_media, nn, media, max_lag), lag_weights);
     X_media_adstocked[nn, media] <- log1p(cum_effect/mu_mdip[media]);
    }
  X <- append_col(X_media_adstocked, X_ctrl);
  } 
}
model {
  decay ~ beta(3,3);
  peak ~ uniform(0, ceil(max_lag/2));
  tau ~ normal(0, 5);
  for (i in 1 : num_media+num_ctrl) {
    beta[i] ~ normal(0, 1);
  }
  noise_var ~ inv_gamma(0.05, 0.05 * 0.01);
  y ~ normal(tau + X * beta, sqrt(noise_var));
}
"

# Compile and run the Stan model
sm2 <- stan_model(model_code = model_code2)
fit2 <- sampling(sm2, data = model_data2, iter = n_iter, chains = n_chains)

# Extract results from the Marketing Mix Model
fit2_result <- rstan::extract(fit2)

# OK

##################### Section: Function to Extract MMM Parameters #####################
# Define a function to extract parameters from the MMM fit results
extract_mmm <- function(fit_result, max_lag, media_vars, ctrl_vars = c('base_sales'), extract_param_list = TRUE) {
  mmm <- list()
  mmm$max_lag <- max_lag
  mmm$media_vars <- media_vars
  mmm$ctrl_vars <- ctrl_vars
  mmm$decay <- colMeans(fit_result$decay)
  mmm$peak <- colMeans(fit_result$peak)
  mmm$beta <- colMeans(fit_result$beta)
  mmm$tau <- mean(fit_result$tau)
  
  if (extract_param_list) {
    mmm$decay_list <- fit_result$decay
    mmm$peak_list <- fit_result$peak
    mmm$beta_list <- fit_result$beta
    mmm$tau_list <- fit_result$tau
  }
  
  adstock_params <- list()
  media_names <- gsub("mdip_", "", media_vars)
  for (i in seq_along(media_names)) {
    adstock_params[[media_names[i]]] <- list(
      L = max_lag,
      P = mmm$peak[i],
      D = mmm$decay[i]
    )
  }
  mmm$adstock_params <- adstock_params
  
  return(mmm)
}

# Extract parameters using the defined function
mmm <- extract_mmm(fit2_result, max_lag = max_lag, media_vars = mdip_cols, ctrl_vars = c('base_sales'))

##################### Section: Save Extracted Parameters #####################
# Save the extracted MMM parameters to a JSON file
write_json(mmm, "mmm1.json")

##################### Section: Plot Media Coefficients' Distributions #####################
# Extract beta coefficients for media variables and plot their distributions
beta_media <- lapply(seq_along(mmm$media_vars), function(i) {
  md <- mmm$media_vars[i]
  betas <- sapply(mmm$beta_list, function(beta) beta[i])
  return(betas)
})
names(beta_media) <- mmm$media_vars

# Generate density plots for each media variable's coefficients
plots <- lapply(seq_along(mmm$media_vars), function(i) {
  md <- mmm$media_vars[i]
  x <- beta_media[[md]]
  mean_x <- mean(x)
  median_x <- median(x)
  
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = "lightblue", alpha = 0.7) +
    geom_vline(xintercept = mean_x, color = "red", linetype = "solid") +
    geom_vline(xintercept = median_x, color = "green", linetype = "solid") +
    ggtitle(md) +
    theme_minimal()
})

# Arrange the generated plots in a grid layout
grid.arrange(grobs = plots, ncol = 3)

##################### Section: Data Preparation for Sales Decomposition #####################
# Prepare data structure for decomposing sales to media channels' contribution
media_contribution <- list()
for (md in mmm$media_vars) {
  media_contribution[[md]] <- numeric(nrow(df))  # Initialize with zeros
}

# Print structures for debugging
print(str(mmm))
print(str(df))

# OK

##################### Section: Decompose Sales to Media Contributions #####################
# Define a function to decompose sales to contributions from individual media channels
mmm_decompose_contrib <- function(mmm, df, original_sales = df$sales) {
  cat("Starting mmm_decompose_contrib\n")
  
  tryCatch({
    # Adstock parameters
    adstock_params <- mmm$adstock_params
    if (is.null(adstock_params)) stop("Adstock parameters are missing from mmm object")
    cat("Adstock params extracted\n")
    
    # Extract model coefficients and intercept
    beta <- mmm$beta
    tau <- mmm$tau
    if (is.null(beta) || is.null(tau)) stop("Beta or tau is missing from mmm object")
    cat("Beta and tau extracted\n")
    
    # Variables
    media_vars <- mmm$media_vars
    ctrl_vars <- mmm$ctrl_vars
    if (is.null(media_vars) || is.null(ctrl_vars)) stop("Media or control variables are missing from mmm object")
    num_media <- length(media_vars)
    num_ctrl <- length(ctrl_vars)
    cat("Variables extracted\n")
    
    # Apply adstock transformation
    X_media2 <- adstock_transform(df, media_vars, adstock_params)
    cat("Adstock transformation applied\n")
    
    # Mean-center and log1p transform
    result <- mean_log1p_transform(X_media2, media_vars)
    X_media2 <- result$df_new + 1
    sc_mmm2 <- result$sc
    cat("Mean-centering and log1p transformation applied\n")
    
    # Process control variables
    result <- mean_log1p_transform(df[ctrl_vars], ctrl_vars)
    X_ctrl2 <- result$df_new + 1
    sc_mmm2 <- c(sc_mmm2, result$sc)
    cat("Control variables processed\n")
    
    # Process sales data
    result <- mean_log1p_transform(df["sales"], "sales")
    y_true2 <- result$df_new + 1
    sc_mmm2 <- c(sc_mmm2, result$sc)
    cat("Sales data processed\n")
    
    # Combine media and control variables
    X2 <- cbind(X_media2, X_ctrl2)
    
    # Calculate contributions
    factor_df <- data.frame(matrix(ncol = length(c(media_vars, ctrl_vars, "intercept")), nrow = nrow(df)))
    colnames(factor_df) <- c(media_vars, ctrl_vars, "intercept")
    
    for (i in 1:num_media) {
      colname <- media_vars[i]
      factor_df[[colname]] <- X2[[colname]] ^ beta[i]
    }
    
    for (i in 1:num_ctrl) {
      colname <- ctrl_vars[i]
      factor_df[[colname]] <- X2[[colname]] ^ beta[num_media + i]
    }
    
    factor_df$intercept <- exp(tau)
    
    # Calculate predicted values and baseline
    y_pred <- apply(factor_df, 1, prod)
    factor_df$y_pred <- y_pred
    factor_df$y_true2 <- y_true2
    factor_df$baseline <- apply(factor_df[c("intercept", ctrl_vars)], 1, prod)
    
    # Calculate media contributions
    mc_df <- data.frame(matrix(ncol = length(c(media_vars, "baseline")), nrow = nrow(df)))
    colnames(mc_df) <- c(media_vars, "baseline")
    
    for (col in media_vars) {
      mc_df[[col]] <- factor_df$y_true2 - factor_df$y_true2 / factor_df[[col]]
    }
    
    mc_df$baseline <- factor_df$baseline
    mc_df$y_true2 <- factor_df$y_true2
    
    # Scale contributions
    mc_df$mc_pred <- rowSums(mc_df[media_vars])
    mc_df$mc_true <- mc_df$y_true2 - mc_df$baseline
    mc_df$mc_delta <- mc_df$mc_pred - mc_df$mc_true
    
    for (col in media_vars) {
      mc_df[[col]] <- mc_df[[col]] - mc_df$mc_delta * mc_df[[col]] / mc_df$mc_pred
    }
    
    # Scale based on original sales
    mc_df$sales <- original_sales
    for (col in c(media_vars, "baseline")) {
      mc_df[[col]] <- mc_df[[col]] * mc_df$sales / mc_df$y_true2
    }
    
    cat("Finished mmm_decompose_contrib\n")
    return(mc_df)
    
  }, error = function(e) {
    cat("Error in mmm_decompose_contrib:", e$message, "\n")
    return(NULL)
  })
}



##################### Section: Adstock Transformation Function #####################
# Define a function for applying adstock transformation to media variables based on specified parameters
adstock_transform <- function(df, media_vars, adstock_params) {
  result <- data.frame(matrix(nrow = nrow(df), ncol = length(media_vars)))
  colnames(result) <- media_vars
  
  for (media in names(adstock_params)) {
    col <- paste0("mdip_", media)
    if (!(col %in% names(df))) {
      stop("Column '", col, "' not found in the input data frame")
    }
    x <- df[[col]]
    L <- adstock_params[[media]]$L
    P <- adstock_params[[media]]$P
    D <- adstock_params[[media]]$D
    result[[col]] <- apply_adstock(x, L, P, D)
  }
  
  return(result)
}

# OK

##################### Section: Calculate Media Contribution Percentage #####################
# Define a function to calculate media contribution as a percentage of total sales over a specified period
calc_media_contrib_pct <- function(mc_df, media_vars = mdip_cols, sales_col = 'sales', period = 52) {
  if (is.null(mc_df) || !is.data.frame(mc_df)) {
    cat("Error: mc_df is not a valid data frame\n")
    return(NULL)
  }
  
  if (!all(c(media_vars, 'baseline', sales_col) %in% names(mc_df))) {
    cat("Error: mc_df is missing required columns\n")
    return(NULL)
  }
  
  mc_pct <- list()
  mc_pct2 <- list()
  s <- 0
  
  tryCatch({
    if (is.null(period)) {
      for (col in c(media_vars, 'baseline')) {
        mc_pct[[col]] <- mean(mc_df[[col]] / mc_df[[sales_col]])
      }
    } else {
      for (col in c(media_vars, 'baseline')) {
        mc_pct[[col]] <- mean(tail(mc_df[[col]] / mc_df[[sales_col]], period))
      }
    }
    
    for (m in media_vars) {
      s <- s + mc_pct[[m]]
    }
    
    for (m in media_vars) {
      mc_pct2[[m]] <- mc_pct[[m]] / s
    }
    
    return(list(mc_pct = mc_pct, mc_pct2 = mc_pct2))
  }, error = function(e) {
    cat("Error in calc_media_contrib_pct:", e$message, "\n")
    return(NULL)
  })
}

# Extract and calculate media contribution from decomposed data
mc_df <- mmm_decompose_contrib(mmm, df, original_sales = df$sales)
if (!is.null(mc_df)) {
  adstock_params <- mmm$adstock_params
  result <- calc_media_contrib_pct(mc_df, period = 52)
  if (!is.null(result)) {
    mc_pct <- result$mc_pct
    mc_pct2 <- result$mc_pct2
    cat("Media contribution percentages calculated successfully.\n")
  } else {
    cat("Failed to calculate media contribution percentages.\n")
  }
} else {
  cat("Failed to decompose media contributions.\n")
}

# Save media contribution results
write.csv(mc_df, "mc_df1.csv", row.names = FALSE)
write_json(adstock_params, "adstock_params1.json")
mc_pct_df <- data.frame(
  mc_pct = unlist(mc_pct),
  mc_pct2 = c(unlist(mc_pct2), rep(NA, length(mc_pct) - length(mc_pct2)))
)
write.csv(mc_pct_df, "mc_pct_df1.csv", row.names = TRUE)

##################### Section: Plot Media Contribution Across Time #####################
# Load and prepare data for plotting
head(mc_df, 3)
str(mc_df)
columns <- c('y_true2', 'mc_pred', 'mc_true', 'mc_delta', 'sales')
df_temp <- mc_df[, !(names(mc_df) %in% columns)]
head(df_temp, 3)

# Determine the number of data rows for plotting
data_rows <- nrow(df_temp)
if (data_rows == 0) {
  stop("df_temp has no rows")
}

x <- 1:data_rows
y <- as.matrix(df_temp)
y2 <- t(y)

##################### Section: Dynamic Dimension Setting Function #####################
# Define a function to dynamically adjust dimensions based on data content
set_dimension <- function(dims, index, value) {
  if (length(dims) > index) {
    if (dims[index] < value) {
      dims[index] <- value
    }
  } else {
    dims <- c(dims, value)
  }
  return(dims)
}

list1 <- split(y2, row(y2))
dims <- c()
dims <- set_dimension(dims, 1, length(list1))

for (i in seq_along(list1)) {
  if (is.list(list1[[i]])) {
    dims <- set_dimension(dims, 2, length(list1[[i]]))
    for (j in seq_along(list1[[i]])) {
      if (is.list(list1[[i]][[j]])) {
        dims <- set_dimension(dims, 3, length(list1[[i]][[j]]))
        for (k in seq_along(list1[[i]][[j]])) {
          if (is.list(list1[[i]][[j]][[k]])) {
            dims <- set_dimension(dims, 4, length(list1[[i]][[j]][[k]]))
            print('four dimensions')
          }
        }
      }
    }
  }
}

print(paste('number of dimensions', length(dims)))
print(paste('maximum shape', paste(dims, collapse = " ")))

# OK

# debug 
str(df_temp)

if (!"date" %in% names(df_temp)) {
  df_temp$date <- as.Date(rownames(df_temp))  # or however you want to create the date column
}

##################### Section: Data Visualization #####################

# First, simplify the structure of df_temp
simplify_df <- function(df) {
  result <- data.frame(matrix(nrow = nrow(df), ncol = ncol(df)))
  colnames(result) <- names(df)
  for (col in names(df)) {
    result[[col]] <- df[[col]]$sales
  }
  return(result)
}

df_temp_simplified <- simplify_df(df_temp)

# Add a date column (replace this with actual date data if available)
df_temp_simplified$date <- seq(as.Date("2014-01-01"), by = "week", length.out = nrow(df_temp_simplified))

# Melt the dataframe for visualization
library(reshape2)
df_long <- melt(df_temp_simplified, id.vars = "date")

# Create a stacked area plot for media variables' contributions over time
library(ggplot2)
media_vars <- setdiff(names(df_temp_simplified), c("baseline", "date"))
df_media <- df_temp_simplified[c("date", media_vars)]
df_media_long <- melt(df_media, id.vars = "date")

media_plot <- ggplot(data = df_media_long, aes(x = date, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Media Contribution Across Time", 
       x = "Date", 
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +  # Changed from scale_color_brewer to scale_fill_brewer
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 months")

# Save the plot to file
ggsave("media_contribution_plot.png", media_plot, width = 15, height = 10, dpi = 300)

# Print the path where the file is saved
print(paste("Plot saved as:", file.path(getwd(), "media_contribution_plot.png")))

# Create a separate plot for baseline
baseline_plot <- ggplot(data = df_temp_simplified, aes(x = date, y = baseline)) +
  geom_line() +
  labs(title = "Baseline Over Time", 
       x = "Date", 
       y = "Baseline Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 months")

ggsave("baseline_plot.png", baseline_plot, width = 15, height = 10, dpi = 300)
print(paste("Baseline plot saved as:", file.path(getwd(), "baseline_plot.png")))