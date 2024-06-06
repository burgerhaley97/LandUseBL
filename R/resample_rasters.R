# Install and load necessary package
library(terra)

# try with just a bounding box around bear lake county ID, terra::values()
# needs more memory than I have for the whole raster

BL_county <- tigris::counties(state = "ID", cb = TRUE) %>%
  st_as_sf() %>%
  filter(NAME %in% "Bear Lake")
class(BL_county)

BL_county <- st_transform(BL_county, crs = st_crs(ag_raster_2008))
county_crop <- crop(ag_raster_2008, BL_county)
plot(county_crop)


# Function to randomly select a percentage of pixels with a specific value and
# changes their values based on specified probabilities
randomly_select_pixels <- function(input_raster, target_value, prob_transition = 0.1, prob_0 = 0.7, prob_1 = 0.3) {
  # Get the raster values
  raster_values <- values(input_raster)

  # Identify the cells that match the target value
  target_cells <- which(raster_values == target_value)

  # Calculate the number of pixels to select (percentage of the target cells)
  num_to_select <- ceiling(length(target_cells) * prob_transition)

  # Randomly select the target cells
  selected_cells <- sample(target_cells, num_to_select)

  # Determine the new values based on the specified probabilities
  new_values_selected <- sample(c(0, 1), size = num_to_select, replace = TRUE, prob = c(prob_0, prob_1))

  # Create a new vector to store the new values
  new_values <- raster_values

  # Change the values of the selected cells to the new values
  new_values[selected_cells] <- new_values_selected

  # Assign new values to the output raster
  output_raster <- input_raster
  values(output_raster) <- new_values

  return(output_raster)
}

test_sample <- randomly_select_pixels(county_crop, target_value = 1, prob_transition = 0.3)
plot(test_sample)

# Simulation function
simulate_raster_changes <- function(input_raster, target_value,
                            prob_transition = 0.1, prob_0 = 0.7, prob_1 = 0.3,
                            n_simulations = 100) {
  result_counts <- numeric(n_simulations)

  raster_values <- values(input_raster)

  # Identify the cells that match the target value
  target_cells <- which(raster_values == target_value)

  # Calculate the number of pixels to select (percentage of the target cells)
  total_target <- length(target_cells)

  for (i in 1:n_simulations) {
    # Apply the function to get the modified raster
    modified_raster <- randomly_select_pixels(input_raster,
                                              target_value, prob_transition,
                                              prob_0, prob_1)

    # Count the number of cells with the target value after modification
    modified_values <- values(modified_raster)
    result_counts[i] <- sum(modified_values == target_value)
  }

  return((result_counts/total_target) * 100 )
}

# Run the simulation for
set.seed(47)  # For reproducibility
simulation_results <- simulate_raster_changes(county_crop, target_value = 0,
                                  prob_transition = 0.1, prob_0 = 0.7,
                                  prob_1 = 0.3, n_simulations = 1000)

simulation_results_1 <- simulate_raster_changes(county_crop, target_value = 0,
                                              prob_transition = 0.1, prob_0 = 0.7,
                                              prob_1 = 0.3, n_simulations = 100)
# Display the results
hist(simulation_results_1, breaks = 20,
     main = "Distribution of Pixels from Class 0 \n After 100 Simulations",
     xlab = "(simulation cell number/actual cell number) * 100", ylab = "Frequency")


# Run the simulation
simulation_results <- simulate_raster_changes(county_crop, target_value = 0,
                                              prob_transition = 0.1, prob_0 = 0.7,
                                              prob_1 = 0.3, n_simulations = 1000)

