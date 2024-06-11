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
     xlab = "(simulation cell number/actual cell number) * 100", ylab = "Prob", freq = FALSE)


# Run the simulation
simulation_results <- simulate_raster_changes(county_crop, target_value = 0,
                                              prob_transition = 0.1, prob_0 = 0.7,
                                              prob_1 = 0.3, n_simulations = 1000)
hist(simulation_results, breaks = 20,
     main = "Distribution of Pixels from Class 0 \n After 100 Simulations",
     xlab = "number of cells", ylab = "Prob", freq = FALSE, probability = TRUE)

# Run the simulation
simulation_results <- simulate_raster_changes(county_crop, target_value = 0,
                                              prob_transition = 0.1, prob_0 = 0.7,
                                              prob_1 = 0.3, n_simulations = 1000)
hist(simulation_results, breaks = 20,
     main = "Distribution of Pixels from Class 0 \n After 100 Simulations",
     xlab = "number of cells", ylab = "Prob", freq = FALSE, probability = TRUE)


##############################################################################
#                   Try transitioning both values at once
##############################################################################


# Function to randomly select a percentage of pixels with specific values and
# change their values based on specified probabilities
randomly_select_pixels <- function(input_raster, target_values, prob_transitions, prob_0s, prob_1s) {
  # Get the raster values
  raster_values <- values(input_raster)

  # Create a new vector to store the new values
  new_values <- raster_values

  for (i in seq_along(target_values)) {
    target_value <- target_values[i]
    prob_transition <- prob_transitions[i]
    prob_0 <- prob_0s[i]
    prob_1 <- prob_1s[i]

    # Identify the cells that match the target value
    target_cells <- which(raster_values == target_value)

    # Calculate the number of pixels to select (percentage of the target cells)
    num_to_select <- ceiling(length(target_cells) * prob_transition)

    if (num_to_select > 0) {
      # Randomly select the target cells
      selected_cells <- sample(target_cells, num_to_select)

      # Determine the new values based on the specified probabilities
      new_values_selected <- sample(c(0, 1), size = num_to_select, replace = TRUE, prob = c(prob_0, prob_1))

      # Change the values of the selected cells to the new values
      new_values[selected_cells] <- new_values_selected
    }
  }

  # Assign new values to the output raster
  output_raster <- input_raster
  values(output_raster) <- new_values

  return(output_raster)
}

# Example usage

# Define the target values and their respective probabilities
target_values <- c(0, 1)  # Target values to reassign
prob_transitions <- c(0.1, 0.3)  # Probabilities of transition for each target value
prob_0s <- c(0.9, 0.3)  # Probabilities of transitioning to 0 for each target value
prob_1s <- c(0.1, 0.7)  # Probabilities of transitioning to 1 for each target value

# Apply the function
set.seed(47)  # For reproducibility
modified_raster <- randomly_select_pixels(ag_raster_all[["2008"]], target_values, prob_transitions, prob_0s, prob_1s)
plot(modified_raster)

# Plot the original and modified rasters
par(mfrow = c(1, 2))
plot(ag_raster_all[["2008"]], main = "Original Raster")
plot(modified_raster, main = "Modified Raster")


##############################################################################
#                       Simulate Transitions 100 times
##############################################################################

# Function to simulate raster changes over multiple simulations and create a new SpatRaster object
simulate_raster_changes <- function(input_raster, target_values, prob_transitions, prob_0s, prob_1s, n_simulations = 100) {
  # Create a character vector to store paths of simulation result files
  result_files <- character(n_simulations)

  for (i in 1:n_simulations) {
    # Apply the function to get the modified raster
    modified_raster <- randomly_select_pixels(input_raster, target_values, prob_transitions, prob_0s, prob_1s)

    # Write the modified raster to a temporary file
    temp_file <- tempfile(fileext = ".tif")
    suppressMessages(writeRaster(modified_raster, temp_file, overwrite = TRUE))

    # Store the path to the temporary file
    result_files[i] <- temp_file
  }
    # Create a SpatRaster from the temporary files
    simulation_results <- rast(result_files)

  return(simulation_results)
}

# Define the target values and their respective probabilities
target_values <- c(0, 1)  # Target values to reassign
prob_transitions <- c(0.1, 0.3)  # Probabilities of transition for each target value
prob_0s <- c(0.9, 0.3)  # Probabilities of transitioning to 0 for each target value
prob_1s <- c(0.1, 0.7)  # Probabilities of transitioning to 1 for each target value

# Run the simulation
set.seed(47)  # For reproducibility
simulation_results <- simulate_raster_changes(county_crop, target_values, prob_transitions, prob_0s, prob_1s, n_simulations = 100)

# Plot the first simulation result
plot(simulation_results)

simulation_results_bl <- simulate_raster_changes(ag_raster_all[["2008"]], target_values, prob_transitions, prob_0s, prob_1s, n_simulations = 100)

# Plot the first simulation result
plot(simulation_results_bl[[100]])

# Define the path where you want to save the file
file_path <- "Intermediate_rasters/simulation_results_bl.tif"

# Write the SpatRaster to a file
terra::writeRaster(simulation_results_bl, filename = file_path)



# try getting mean patch area distribution on sim data
# mean area of patches in each class
# Initialize a list to store landscape metrics for each year
landscape_metrics_list <- list()

reclassified_raster <- simulation_results_bl

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metricshttp://127.0.0.1:41279/graphics/plot_zoom_png?width=1792&height=697
  landscape_metrics <- lsm_c_area_mn(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_list[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
c_area_metrics <- bind_rows(landscape_metrics_list)
c_area_metrics

# Add a new column with repeated numbers
c_area_metrics$sim_number <- rep(1:100, each = 2)

# Plot changes in mean patch area over time
library(ggplot2)

class_labels <- c("0" = "Non-Ag", "1" = "Ag")
ggplot(c_area_metrics, aes(x = as.numeric(sim_number), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "sim number",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()


c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% 1)
c_area_metrics_sub

ggplot(c_area_metrics_sub, aes(x = as.numeric(sim_number), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "sim number",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = "green", labels = class_labels) +
  theme_minimal()

