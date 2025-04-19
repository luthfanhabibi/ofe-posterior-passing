# Load required libraries
library(tidyverse)
library(loo)
library(doParallel)
library(foreach)
library(doSNOW)

# Load data
data <- read_delim("./output/combined_data.csv", 
                   escape_double = FALSE, trim_ws = TRUE)

data$year = as.factor(data$year) 
data$field.id = as.factor(data$field.id) 
data = na.omit(data) # Remove NA

# Define the field IDs and models to process
field_ids <- c(1, 2)
model_names <- c("noninf_", "inf_")

# Loop through models and field IDs
for (model_name in model_names) {
  for (field_id in field_ids) {
    # Filter data for the current field ID
    data_field <- data %>% filter(field.id == field_id)
    
    # Load the posterior from the corresponding RDS file
    fit <- readRDS(paste0("./output/gp_model_rds/fit_", model_name, field_id, ".rds"))
    posterior <- as.data.frame(fit)
    
    # Prepare variables
    y <- data_field$yield.total
    S <- nrow(posterior)
    coord <- as.matrix(data_field[, c("x_53n", "y_53n")])
    dist <- as.matrix(dist(coord, method = "euclidean"))
    X <- matrix(0, nrow(data_field), 3)
    X[, 1] <- 1 # intercept
    X[, 2] <- (data_field$target.dens)^2
    X[, 3] <- data_field$target.dens
    
    # Initialize parallel processing
    num_cores <- detectCores()
    cl <- makeCluster(num_cores - 1)
    registerDoParallel(cl)
    registerDoSNOW(cl)
    
    pb <- txtProgressBar(max = S, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    # Function to process data in parallel
    process_data <- function(posterior, y, X, coord, chunk_id) {
      N <- length(y)
      p <- posterior[chunk_id, ]
      eta <- c(p$"beta[1]", p$"quadcoef", p$"firstcoef")
      C <- p$"alpha"^2 * exp(-0.5 * ((dist / p$"rho")^2))
      diag(C) <- p$"sigma" + p$"alpha"^2
      Cinv <- solve(C)
      
      predictions <- c()
      for (n in 1:N) {
        n_new <- 1
        data_new <- coord[n, ]
        original <- matrix(rep(coord[n, ], N - 1), ncol = 2, byrow = TRUE)
        distances <- sqrt(rowSums((coord[-n, ] - original)^2))
        K_star <- p$"alpha"^2 * exp(-0.5 * (distances / p$"rho")^2)
        
        residual <- y[-n] - X[-n, ] %*% eta
        prediction_mean <- X[n, ] %*% eta + t(solve(C[-n, -n]) %*% K_star) %*% residual
        predictions <- c(predictions, prediction_mean)
      }
      
      g <- Cinv %*% (y - predictions)
      cbar <- diag(Cinv)
      yloo_ <- y - g / cbar
      sdloo_ <- sqrt(1 / cbar)
      loglik_ <- dnorm(y, yloo_, sdloo_, log = TRUE)
      
      write.table(matrix(predictions, nrow = 1), file = paste0("./output/temp/pred_", chunk_id, ".txt"))
      write.table(matrix(yloo_, nrow = 1), file = paste0("./output/temp/yloo_", chunk_id, ".txt"))
      write.table(matrix(sdloo_, nrow = 1), file = paste0("./output/temp/sdloo_", chunk_id, ".txt"))
      write.table(matrix(loglik_, nrow = 1), file = paste0("./output/temp/loglik_", chunk_id, ".txt"))
    }
    
    # Perform computations in parallel
    foreach(s = 1:S, .combine = 'c', .options.snow = opts) %dopar% {
      process_data(posterior, y, X, coord, s)
    }
    
    # Close parallel processing
    close(pb)
    stopCluster(cl)
    
    # Combine output files
    output_combined <- "loglik.txt"
    for (i in 1:S) {
      output_file <- paste0("output/temp/loglik_", i, ".txt")
      a <- read.table(output_file)
      write.table(a, file = output_combined, append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    output <- read.table(output_combined)
    saveRDS(output, file = paste0("output/loo/loglik_", model_name, field_id, ".rds"))
    file.remove(output_combined)
  }
}

# Finalization
# Generate all combinations of model_names and field_ids
combinations <- expand.grid(model_name = model_names, field_id = field_ids)

# Load log_ratios for all combinations
log_ratios_list <- lapply(1:nrow(combinations), function(i) {
  model_name <- combinations$model_name[i]
  field_id <- combinations$field_id[i]
  file_path <- paste0("output/loo/loglik_", model_name, field_id, ".rds")
  
  if (file.exists(file_path)) {
    -(as.matrix(readRDS(file_path)))
  } else {
    warning(paste("File not found:", file_path))
    NULL
  }
})

r_eff_list <- lapply(log_ratios_list, function(log_ratios) {
  relative_eff(exp(log_ratios), chain_id = c(rep(1, 500), rep(2, 500), rep(3, 500), rep(4, 500)))
})

psis_loo_list <- mapply(function(log_ratios, r_eff) {
  loo(log_ratios, r_eff = r_eff, cores = 2)
}, log_ratios_list, r_eff_list, SIMPLIFY = FALSE)

waic_list <- mapply(function(log_ratios, r_eff) {
  waic(log_ratios, r_eff = r_eff, cores = 2)
}, log_ratios_list, r_eff_list, SIMPLIFY = FALSE)

# Compare models
loo_diff_list <- loo_compare(psis_loo_list[[1]], psis_loo_list [[2]], psis_loo_list [[3]], psis_loo_list [[4]])
waic_diff_list <- loo_compare(waic_list[[1]], waic_list [[2]], waic_list[[3]], waic_list [[4]])

# Print WAIC estimates
lapply(waic_list, function(waic) waic$estimates)
