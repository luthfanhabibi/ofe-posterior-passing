library(tidyverse)
library(rstan)

rstan_options(auto_write = TRUE)

# Load the data
data <- read_delim("./output/combined_data.csv", 
                   escape_double = FALSE, trim_ws = TRUE)

# Preparing the data
data$year <- as.factor(data$year)
data$field.id <- as.factor(data$field.id)
data <- na.omit(data) # Remove NA
str(data)

# Define the original Stan model as a string
stan_code <- "
data {
  int<lower=1> N;
  matrix[N, N] distances;
  int K;                          // number of columns in the model matrix
  vector[N] X0;                   // Treatment
  matrix[N, K] X1;                // the model matrix
  vector[N] y;
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  real<upper=0> quadcoef;
  real<lower=0> firstcoef;
  vector[K] beta;
}
model {
  matrix[N, N] COV;
  COV = square(alpha) * exp(-0.5 * (square(distances / rho))) + diag_matrix(rep_vector(sigma, N));
  matrix[N, N] L_K = cholesky_decompose(COV);

  alpha ~ std_normal();
  sigma ~ std_normal();
  rho ~ normal(2, 2.5);
  quadcoef ~ normal(0, 10);         // original prior with noninformative setup
  firstcoef ~ normal(0, 100);       // original prior with noninformative setup
  beta ~ normal(0, 500);            // original prior with noninformative setup
  y ~ multi_normal_cholesky(X1 * beta + X0 * firstcoef + square(X0) * quadcoef, L_K);
}
"

# Define the updated prior Stan model as a string
stan_code_updated_prior <- "
data {
  int<lower=1> N;
  matrix[N, N] distances;
  int K;                          // number of columns in the model matrix
  vector[N] X0;                   // Treatment
  matrix[N, K] X1;                // the model matrix
  vector[N] y;
  real quadcoef_mean;
  real quadcoef_sd;
  real firstcoef_mean;
  real firstcoef_sd;
  real beta1_mean;
  real beta1_sd;
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  real<upper=0> quadcoef;
  real<lower=0> firstcoef;
  vector[K] beta;
}
model {
  matrix[N, N] COV;
  COV = square(alpha) * exp(-0.5 * (square(distances / rho))) + diag_matrix(rep_vector(sigma, N));
  matrix[N, N] L_K = cholesky_decompose(COV);

  alpha ~ std_normal();
  sigma ~ std_normal();
  rho ~ normal(2, 2.5);
  quadcoef ~ normal(quadcoef_mean, quadcoef_sd);          // updated prior from the noninformative model
  firstcoef ~ normal(firstcoef_mean, firstcoef_sd);       // updated prior from the noninformative model
  beta ~ normal(beta1_mean, beta1_sd);                    // updated prior from the noninformative model
  y ~ multi_normal_cholesky(X1 * beta + X0 * firstcoef + square(X0) * quadcoef, L_K);
}
"

# Compile the Stan models
mod <- stan_model(model_code = stan_code)
mod_updated_prior <- stan_model(model_code = stan_code_updated_prior)

# Initialize storage for posterior means and standard deviations
posterior_list <- list()

# Step 1: Run the original Stan model for each field and save posterior means
field_ids <- c(1, 2)

for (field_id in field_ids) {
  # Filter data for the current field ID
  data_stan <- data %>% filter(field.id == field_id)
  
  # Compute Euclidean distances
  dist <- as.matrix(dist(data_stan[, c("x_53n", "y_53n")], method = "euclidean"))
  
  # Define covariates
  K <- 1  # Number of covariates other than treatments (X0 and X1)
  X0 <- data_stan$target.dens  # Change here if using plant density
  X1 <- matrix(0, nrow(data_stan), K)
  X1[, 1] <- 1  # Intercept
  
  # Prepare data for Stan model
  stan_dat <- list(
    distances = dist,
    K = K,
    X0 = X0,
    X1 = X1,
    y = data_stan$yield.total,
    N = length(data_stan$yield.total)
  )
  
  # Fit the Stan model
  set.seed(999)
  fit <- sampling(
    mod, 
    data = stan_dat, 
    cores = parallel::detectCores(),  # Automatically detect the number of cores
    chains = 4, 
    iter = 1000, 
    refresh = 50, 
    control = list(adapt_delta = 0.99, max_treedepth = 10)
  )
  
  # Extract posterior means and standard deviations
  posterior <- extract(fit)
  
  posterior_list[[field_id]] <- list(
    beta1_mean = mean(posterior$beta[, 1]),
    beta1_sd = sd(posterior$beta[, 1]),
    firstcoef_mean = mean(posterior$firstcoef),
    firstcoef_sd = sd(posterior$firstcoef),
    quadcoef_mean = mean(posterior$quadcoef),
    quadcoef_sd = sd(posterior$quadcoef)
  )
  
  # Save the fitted original model as an RDS file
  fit@stanmodel@dso <- new("cxxdso")
  saveRDS(fit, file = paste0("./output/gp_model_rds/fit_noninf_", field_id, ".rds"))
  
  # Clean up the environment
  remove(fit)
}

# Step 2: Run the updated prior Stan model with cross-field posterior priors
for (field_id in field_ids) {
  # Filter data for the current field ID
  data_stan <- data %>% filter(field.id == field_id)
  
  # Compute Euclidean distances
  dist <- as.matrix(dist(data_stan[, c("x_53n", "y_53n")], method = "euclidean"))
  
  # Define covariates
  K <- 1  # Number of covariates other than treatments (X0 and X1)
  X0 <- data_stan$target.dens  # Change here if using plant density
  X1 <- matrix(0, nrow(data_stan), K)
  X1[, 1] <- 1  # Intercept
  
  # Get posterior means and standard deviations from the other field
  other_field_id <- ifelse(field_id == 1, 2, 1)
  posterior_other <- posterior_list[[other_field_id]]
  
  # Prepare data for the updated prior Stan model
  stan_dat_updated_prior <- list(
    distances = dist,
    K = K,
    X0 = X0,
    X1 = X1,
    y = data_stan$yield.total,
    N = length(data_stan$yield.total),
    quadcoef_mean = posterior_other$quadcoef_mean,
    quadcoef_sd = posterior_other$quadcoef_sd,
    firstcoef_mean = posterior_other$firstcoef_mean,
    firstcoef_sd = posterior_other$firstcoef_sd,
    beta1_mean = posterior_other$beta1_mean,
    beta1_sd = posterior_other$beta1_sd
  )
  
  # Fit the updated prior Stan model
  fit_updated_prior <- sampling(
    mod_udpated_prior, 
    data = stan_dat_updated_prior, 
    cores = parallel::detectCores(),  # Automatically detect the number of cores
    chains = 4, 
    iter = 1000, 
    refresh = 50, 
    control = list(adapt_delta = 0.99, max_treedepth = 10)
  )
  
  # Save the fitted updated prior model as an RDS file
  fit_updated_prior@stanmodel@dso <- new("cxxdso")
  saveRDS(fit_updated_prior, file = paste0("./output/gp_model_rds/fit_inf_", field_id, ".rds"))
  
  # Clean up the environment
  remove(fit_updated_prior)
}