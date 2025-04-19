# Load data
data <- read_delim("./output/combined_data.csv", 
                   escape_double = FALSE, trim_ws = TRUE)

data$year <- as.factor(data$year) 
data$field.id <- as.factor(data$field.id) 
data <- na.omit(data) # Remove NA

# Define the field IDs and models to process
field_ids <- c(1, 2)  # Add more field IDs as needed
model_names <- c("noninf_", "inf_")  # Add more model names as needed

# Prepare data subsets and load models
data_sub <- list()
for (i in 1:length(field_ids)) {
  field <- field_ids[i]
  data_sub[[i]] <- data %>% filter(field.id == field)
}

# Loop through each model and field to generate visualizations
for (model_name in model_names) {
  model_list <- list()
  posterior <- list()
  
  # Load models for the current model name
  for (i in 1:length(field_ids)) {
    field <- field_ids[i]
    model_list[[i]] <- readRDS(paste0("./output/gp_model_rds/fit_", model_name, field, ".rds"))
    posterior[[i]] <- as.data.frame(model_list[[i]])
  }
  
  # Generate plots for each field
  for (path in 1:length(field_ids)) {
    S <- nrow(posterior[[path]])
    
    png(file = paste0("output/Figure/yield_resp_", model_name, path, ".png"),
        width = 5, height = 6, units = "in", res = 900)
    
    plot(x = 0, y = 0, type = 'n',
         xlim = c(10, 35), ylim = c(0, 350), xlab = 'Seeding rate', ylab = 'Yield response')
    random_values <- sample(1:2000, size = 350, replace = FALSE)
    dummy_SeedRate <- seq(0, 35, 0.5)
    
    rect(min(data_sub[[path]]$target.dens), -13, max(data_sub[[path]]$target.dens), 363, col = "#F2F2F2", lty = 0, lwd = 0)
    
    for (chunk_id in random_values) {
      p <- posterior[[path]][chunk_id, ]
      eta <- c(p$`beta[1]`, p$quadcoef, p$firstcoef)
      y <- eta[1] + eta[2] * dummy_SeedRate^2 + eta[3] * dummy_SeedRate
      lines(dummy_SeedRate, y, col = rgb(0.8, 0.8, 0.8, alpha = 0.4))
    }
    
    eta <- c(mean(posterior[[path]]$`beta[1]`), mean(posterior[[path]]$quadcoef), mean(posterior[[path]]$firstcoef))
    y <- eta[1] + eta[2] * dummy_SeedRate^2 + eta[3] * dummy_SeedRate
    lines(dummy_SeedRate, y, col = rgb(1, 0, 0, alpha = 0.7), lwd = 3)
    
    points(data_sub[[path]]$target.dens, data_sub[[path]]$yield.total, type = "p", pch = 1, col = "darkgrey", bg = NA, cex = 1)
    
    dev.off()
  }
}