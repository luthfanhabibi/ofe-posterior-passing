library(tidyverse)
library(ggplot2)

#### Generate data for analysis ####
set.seed(123) # For reproducibility

# Function to generate synthetic data for a specific year and field.id
generate_data <- function(n_rows, n_cols, x_min, x_max, y_min, y_max, target_dens_values, year, field_id, yield_params) {
  # Generate grid coordinates
  x_coords <- seq(x_min, x_max, length.out = n_cols)
  y_coords <- seq(y_min, y_max, length.out = n_rows)
  grid <- expand.grid(x_53n = x_coords, y_53n = y_coords)
  
  # Assign target.dens treatments
  grid <- grid %>%
    mutate(
      target.dens = rep(target_dens_values, each = n_rows)
    )
  
  # Generate synthetic data
  data_generated <- grid %>%
    mutate(
      year = year,
      field.id = field_id
    ) %>%
    rowwise() %>%
    mutate(
      yield.total = {
        # Explicitly pass the current value of target.dens to filter
        current_target_dens <- target.dens
        params <- yield_params %>% filter(target.dens == current_target_dens)
        if (nrow(params) != 1) stop("Yield parameters not uniquely matched for target.dens")
        yield <- rnorm(1, mean = params$mean_yield, sd = params$sd_yield)
        pmin(pmax(yield, params$min_yield), params$max_yield) # Clamp yield within min and max
      }
    ) %>%
    ungroup()
  
  return(data_generated)
}

# Yield parameters for Field ID 1
yield_params_2021 <- tibble(
  target.dens = c(12, 16, 20, 24),
  mean_yield = c(82.4, 118, 190, 170),
  sd_yield = c(54.5, 67.3, 81.2, 49.3),
  min_yield = c(4.43, 9.84, 52.5, 82.0),
  max_yield = c(222, 259, 363, 259)
)

# Yield parameters for Field ID 2
yield_params_2023 <- tibble(
  target.dens = c(23, 27, 31, 35),
  mean_yield = c(171, 158, 134, 153),
  sd_yield = c(36.0, 49.6, 31.0, 44.7),
  min_yield = c(104, 110, 101, 94.2),
  max_yield = c(218, 274, 191, 224)
)

# Generate data for 2021 (Field ID 1)
data_2021 <- generate_data(
  n_rows = 8, n_cols = 16,  # Increased grid size
  x_min = 649800, x_max = 650000, 
  y_min = 3898675, y_max = 3898775, 
  target_dens_values = rep(c(12, 16, 20, 24, 16, 12, 24, 20), times = 2), # Define strip design
  year = 2021, field_id = 1, 
  yield_params = yield_params_2021
)

# Generate data for 2023 (Field ID 2)
data_2023 <- generate_data(
  n_rows = 8, n_cols = 16,  # Increased grid size
  x_min = 649802, x_max = 650007, 
  y_min = 3898690, y_max = 3898780, 
  target_dens_values = rep(c(27, 23, 35, 31, 23, 27, 31, 35), times = 2), # Define strip design
  year = 2023, field_id = 2, 
  yield_params = yield_params_2023
)

# Combine the data into one dataframe
data <- bind_rows(data_2021, data_2023)

# Save the combined dataframe as a CSV file
write_csv(data, "./output/combined_data.csv")

# Scatter plot for Field ID 1
ggplot(data %>% filter(field.id == 1), aes(x = x_53n, y = y_53n, color = as.factor(target.dens))) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("blue", "green", "orange", "red"),
    name = "Target Density\n(seed m²)"
  ) +
  labs(
    title = "Point Locations for Field ID 1",
    x = "x_53n",
    y = "y_53n"
  ) +
  theme_minimal() +
  coord_fixed()

# Scatter plot for Field ID 2
ggplot(data %>% filter(field.id == 2), aes(x = x_53n, y = y_53n, color = as.factor(target.dens))) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("blue", "green", "orange", "red"),
    name = "Target Density\n(seed m²)"
  ) +
  labs(
    title = "Point Locations for Field ID 2",
    x = "x_53n",
    y = "y_53n"
  ) +
  theme_minimal() +
  coord_fixed()

# Calculate summary statistics for yield.total
summary_stats <- data %>%
  group_by(field.id, target.dens) %>%
  summarise(
    count = n(),
    mean_yield = mean(yield.total, na.rm = TRUE),
    sd_yield = sd(yield.total, na.rm = TRUE),
    min_yield = min(yield.total, na.rm = TRUE),
    max_yield = max(yield.total, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)

# Histogram plot for yield.total by target.dens and field.id
ggplot(data, aes(x = yield.total, fill = as.factor(target.dens))) +
  geom_histogram(binwidth = 10, color = "black", alpha = 0.7) +
  facet_wrap(field.id ~ target.dens, scales = "free", labeller = label_both) +
  scale_fill_manual(
    values = c("blue", "green", "orange", "red", "pink", "grey", "purple", "yellow"),
    name = "Target Density\n(seed m²)"
  ) +
  labs(
    title = "Histogram of Yield Total by Target Density and Field ID",
    x = "Yield Total",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

# Scatter plot for Field ID 1 with yield.total as the main key
ggplot(data %>% filter(field.id == 1), aes(x = x_53n, y = y_53n, color = yield.total)) +
  geom_point(size = 3) +
  scale_color_gradient(
    low = "blue", high = "red", 
    name = "Yield Total"
  ) +
  labs(
    title = "Yield Distribution for Field ID 1",
    x = "x_53n",
    y = "y_53n"
  ) +
  theme_minimal() +
  coord_fixed()

# Scatter plot for Field ID 2 with yield.total as the main key
ggplot(data %>% filter(field.id == 2), aes(x = x_53n, y = y_53n, color = yield.total)) +
  geom_point(size = 3) +
  scale_color_gradient(
    low = "blue", high = "red", 
    name = "Yield Total"
  ) +
  labs(
    title = "Yield Distribution for Field ID 2",
    x = "x_53n",
    y = "y_53n"
  ) +
  theme_minimal() +
  coord_fixed()