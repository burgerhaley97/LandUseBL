# Install and load necessary package
library(terra)
library(tigris)
library(sf)
library(dplyr)

##############################################################################
#             Prepare Aggregated Data for Simulation
##############################################################################

# try with just a bounding box around bear lake county ID, terra::values()
# needs more memory than I have for the whole raster
BL_county <- tigris::counties(state = "ID", cb = TRUE) %>%
  st_as_sf() %>%
  filter(NAME %in% "Bear Lake")
class(BL_county)

bl_crop <- rast("Intermediate_rasters/bl_crop.tif")

# Aggregate at 90 meter resolution
bl_90_all <- terra::aggregate(bl_crop, fact = 3, fun = "modal")

# reclassify data for Ag vs Non-ag

# Non-ag values
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# Reclassify the raster values into non-ag = 0, ag = 1, and NaN = NaN
ag_raster_90 <- terra::app(bl_90_all, fun = function(x) {
  ifelse(is.nan(x), NaN, ifelse(x %in% ag_mask, 0, 1))
})

BL_county <- st_transform(BL_county, crs = st_crs(ag_raster_90))
county_crop <- crop(ag_raster_90[["2008"]], BL_county)
plot(county_crop)



##############################################################################
#             Start with just simulating one value
##############################################################################

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
                                  prob_1 = 0.3, n_simulations = 100)

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

# Run the simulation for
set.seed(47)  # For reproducibility
simulation_results <- simulate_raster_changes(, target_value = 0,
                                              prob_transition = 0.1, prob_0 = 0.7,
                                              prob_1 = 0.3, n_simulations = 100)




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


# Display the results

c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% 1)
c_area_metrics_sub
hist(c_area_metrics_sub$value, breaks = 20,
     main = "Distribution of Mn Patch Size from Class 1 \n After 100 Simulations",
     xlab = "(simulation cell number/actual cell number) * 100", ylab = "Prob", probability = TRUE)

c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% 0)
c_area_metrics_sub
hist(c_area_metrics_sub$value, breaks = 20,
     main = "Distribution of Mn Patch Size from Class 0 \n After 100 Simulations",
     xlab = "number of cells", ylab = "Prob", freq = FALSE, probability = TRUE)

# Function to count pixels and plot distribution by layer and class
count_pixels_and_plot_histogram <- function(spatraster) {
  # Check if input is a SpatRaster object
  if (!inherits(spatraster, "SpatRaster")) {
    stop("The input must be a SpatRaster object.")
  }

  # Get unique classes and their counts for each layer
  class_counts <- lapply(1:nlyr(spatraster), function(i) {
    layer <- spatraster[[i]]
    freq(layer)
  })

  # Combine counts from all layers and add layer information
  combined_counts <- do.call(rbind, lapply(seq_along(class_counts), function(i) {
    cbind(Layer = i, class_counts[[i]])
  }))

  # Convert to a data frame
  combined_counts <- as.data.frame(combined_counts)
  colnames(combined_counts) <- c("Layer", "Class", "Count")

  return(combined_counts)
}
# Call the function with the example SpatRaster object
counts <- count_pixels_and_plot_histogram(simulation_results_bl)
class(counts)
colnames(counts) <- c("Layer", "Sub_layer", "Class", "Count")


counts_sub <- counts %>%
  filter(Class %in% 1)


hist(counts_sub$Count, probability = TRUE, main = "Class 1 pixel counts per layer")


counts_sub <- counts %>%
  filter(Class %in% 0)


hist(counts_sub$Count, probability = TRUE, main = "Class 0 pixel counts per layer")






# cores cells labeled as 1, edges labeled as 0 ?
# need to check this

cores <- show_cores(county_crop, class = 1, directions = 4)
cores$layer_1
sum(cores$layer_1$data$values == 1)
sum(cores$layer_1$data$values == 0)
sum(cores$layer_1$data$core_label)

unique(cores$layer_1$data$values)


cores <- show_cores(ag_raster_all[["2008"]], class = 1)
cores$layer_1
sum(cores$layer_1$data$values == 1)
sum(cores$layer_1$data$values == 0)
sum(cores$layer_1$data$core_label)

color_map <- c("red", "blue")
names(color_map) <- 0:1
plot(cores$layer_1, col = color_map)

core_area <- lsm_p_core(county_crop)

##############################################################################
#                   Simulate Using Terra App Function
##############################################################################


# Function to preprocess raster to handle NaN values
# Show cores can't handle NaN or Na values from background raster so need to
# change to arbitrary value (5)
preprocess_raster <- function(raster) {
  values(raster)[is.nan(values(raster))] <- 5
  return(raster)
}
Na_raster <- preprocess_raster(ag_raster_90[["2008"]])

## STEP 1: ID edge pixels
# Function to identify and tag edges
tag_edges <- function(raster, edge_depth = 1) {
  # Identify core areas
  core_areas <- show_cores(raster, edge_depth = edge_depth, class = c(0,1))

  # Convert core_areas data to a matrix and filter for edge values
  core_data <- core_areas$layer_1$data

  # Initialize a new raster to tag edges and cores
  tagged_raster <- raster

  # Get cell indices for the edge coordinates
  edge_coords_class_0 <- core_data[core_data$class == 0 & core_data$values == 0, c("x", "y")]
  edge_coords_class_1 <- core_data[core_data$class == 1 & core_data$values == 0, c("x", "y")]

  edge_cells_class_0 <- cellFromXY(raster, as.matrix(edge_coords_class_0))
  edge_cells_class_1 <- cellFromXY(raster, as.matrix(edge_coords_class_1))

  # Tag edges for class 0 as 2
  tagged_raster[edge_cells_class_0] <- 2

  # Tag edges for class 1 as 3
  tagged_raster[edge_cells_class_1] <- 3

  return(tagged_raster)
}

test_tag <- tag_cores_edges(Na_raster)
plot(test_tag)

core_test <- show_cores(Na_raster, class = c(0,1))
core_test

## STEP 2: Change class of pixels based on transition probability

# Difference in transition values? (accuracy of classification at 90 meter res?)
# Potentially could rewrite transition_pixels in RCPP to speed things up as
# suggested from the terra::app documentation
# Vectorized function to change 10% of cells with value 1 to 0 and 30% of other
# cells to 1
transition_pixels <- function(pixel_values) {

  # Change 30% of Ag pixels to Non-ag (0 value)
  pixel_values[pixel_values == 3 & runif(sum(pixel_values == 3)) < 0.3] <- 0

  # Change 10% of Non-Ag pixels to Ag (1 value)
  pixel_values[pixel_values == 2 & runif(sum(pixel_values == 2)) < 0.1] <- 6

  return(pixel_values)

}

# Vectorized function to process a SpatRaster with multiple layers
process_raster <- function(raster) {
  # Check if the input is a SpatRaster
  if (class(raster) == "SpatRaster") {
    # Apply the transition_pixels function to each cell of the raster
    processed_raster <- app(raster, transition_pixels)
    return(processed_raster)
  } else {
    stop("The input is not a SpatRaster object.")
  }

}


## STEP 3: Run the simulation

# Function to simulate raster changes over multiple years/layers
# see if I can remove the part where I replicate the raster and instead just
# reuse the the same input raster??
simulate_raster_changes <- function(input_raster, n_simulations = 100) {

  # Replicate the input raster n_simulations times
  stacked_rasters <- do.call(c, replicate(n_simulations, input_raster, simplify = FALSE))

  # Apply the transition function to each layer in the stack
  output_rasters <- process_raster(stacked_rasters)

  return(output_rasters)
}

# Run the simulation
output_raster <- simulate_raster_changes(test_tag, n_simulations = 100)
plot(output_raster)

# Measure the time taken to process the raster
time_taken <- system.time({
  # Run the simulation
  output_raster <- simulate_raster_changes(test_tag, n_simulations = 100)
})

# Print the time taken
# 343.86
# took around 5.7 minutes to simulate bl area 100 times
print(time_taken)


plot(output_raster[[1:10]])

# Define a custom color palette for the categorical values
custom_colors <- c("blue", "lightgreen", "red", "yellow", "gray")

# Plot the SpatRaster with custom colors
plot(output_raster[[1]], col = custom_colors, legend = TRUE)

# Add a legend with custom labels
legend("bottomright", legend = c("Non-ag", "Ag", "Non-ag-Edge", "Ag-Edge", "background"), fill = custom_colors)

###############################################################################
# Figure out how to get the values of the k nearest neighbors for the edge cells

# test this out with the county crop raster to start
tag_crop <- tag_cores_edges(county_crop)

# Run the simulation
# define intermediate value that tagged cells can be identified as (6)
crop_raster <- simulate_raster_changes(tag_crop, n_simulations = 5)
plot(crop_raster[[1]])
crop_raster_sub <- crop_raster[[1]]

# get a vector of the tagged edge cells that I want to transition
# (small percent of actual edge cells)
# Specify the value you're interested in
specific_value <- 6

# Get the cell numbers of cells with the specific value
cells_of_interest <- which(values(crop_raster_sub) == specific_value)

# Find the indices of the 4 nearest neighbors for each cell of interest
neighbors <- adjacent(crop_raster_sub, cells=cells_of_interest, directions="rook")

# Get the cell indices of the neighbors
neighbor_indices <- neighbors[,2]


# Extract the values of the nearest neighbors using values()
neighbor_values <- values(crop_raster_sub)[neighbor_indices]


# Combine the cell indices and their values
results <- data.frame(cell = neighbor_indices, value = neighbor_values)
print(results)
