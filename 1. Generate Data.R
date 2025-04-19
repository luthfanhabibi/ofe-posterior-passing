library(tidyverse)
library(ggplot2)

#### Generate data for analysis ####
set.seed(123) # For reproducibility

# Function to generate synthetic data for a specific year and field.id
generate_data <- function(n_rows, n_cols, x_min, x_max, y_min, y_max, target_dens_values, year, field_id, yield_min, yield_max, yield_q1, yield_q3) {
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
      field.id = field_id,
      yield.total = runif(nrow(grid), min = yield_min, max = yield_max)
    ) %>%
    mutate(
      yield.total = case_when(
        yield.total < yield_q1 ~ yield_q1,
        yield.total > yield_q3 ~ yield_q3,
        TRUE ~ yield.total
      )
    )
  
  return(data_generated)
}

# Generate data for 2021 (Field ID 1)
data_2021 <- generate_data(
  n_rows = 8, n_cols = 16,  # Increased grid size
  x_min = 649800, x_max = 650000, 
  y_min = 3898675, y_max = 3898775, 
  target_dens_values = c(12, 20, 16, 24, 20, 12, 16, 24, 12, 20, 16, 24, 20, 12, 16, 24), # Define strip design
  year = 2021, field_id = 1, 
  yield_min = 4.431, yield_max = 362.824, 
  yield_q1 = 4.431, yield_q3 = 362.824
)

# Generate data for 2023 (Field ID 2)
data_2023 <- generate_data(
  n_rows = 8, n_cols = 16,  # Increased grid size
  x_min = 649802, x_max = 650007, 
  y_min = 3898690, y_max = 3898780, 
  target_dens_values = c(27, 23, 35, 31, 23, 35, 27, 31, 27, 23, 35, 31, 23, 35, 27, 31), # Define strip design
  year = 2023, field_id = 2, 
  yield_min = 94.24, yield_max = 273.63, 
  yield_q1 = 114.46, yield_q3 = 178.07
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

