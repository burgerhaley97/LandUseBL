# load packages
library(lattice)
library(terra)
library(magick)
library(dplyr)
library(CropScapeR)
install.packages("landscapemetrics")
library(landscapemetrics)
library(stringr)

# load in cropped data
bl_crop <- rast("Intermediate_rasters/bl_crop.tif")
bl_crop

# try aggregating based on the mode of just one layer
bl_crop_test <- bl_crop[["2008"]]
bl_90 <- terra::aggregate(bl_crop_test, fact = 3, fun = "modal")

# check to make sure that it looks correct
plot(bl_90)

##############################################################################
#                     Aggregate and reclassify data
##############################################################################

# Aggregate at 90 meter resolution
bl_90_all <- terra::aggregate(bl_crop, fact = 3, fun = "modal")

# reclassify data for Ag vs Non-ag

# Non-ag values
ag_mask <- c(61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 181, 190, 195)

# alfalfa and other hay
alfalfa <- c(36:37)

# corn, soy, cotton, wheat (and mixed categories)
major_ag <- c(1, 2, 5, 12, 13, 22:24, 26, 225:226, 228, 234, 236, 238:241, 254)

# Reclassify the raster values into non-ag = 0, ag = 1, and NaN = NaN
ag_raster_90 <- terra::app(bl_90_all, fun = function(x) {
  ifelse(is.nan(x), NaN, ifelse(x %in% ag_mask, 0, 1))
})

# reclassify into non_ag = 0,  ag = 1, major_ag = 2, and NaN = NaN
ag_raster_90_maj <- terra::app(bl_90_all, fun = function(x) {
  ifelse(is.nan(x), NaN,
         ifelse(x %in% major_ag, 2,
                ifelse(x %in% alfalfa, 3,
                       ifelse(x %in% ag_mask, 0, 1))))
})




###############################################################################
# Landscape summary of patch level metrics (Ag vs Non-ag)
###############################################################################

# mean area of patches in each class
# Initialize a list to store landscape metrics for each year
landscape_metrics_list <- list()

reclassified_raster <- ag_raster_90

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_area_mn(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_list[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
c_area_metrics <- bind_rows(landscape_metrics_list)
c_area_metrics

# Plot changes in mean patch area over time
library(ggplot2)

class_labels <- c("0" = "Non-Ag", "1" = "Ag")
ggplot(c_area_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()

c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% 1)
c_area_metrics_sub

ggplot(c_area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = "green", labels = class_labels) +
  theme_minimal()

# describes percentage of landscape belonging to each class, measure of
# composition
# Initialize a list to store landscape metrics for each year
landscape_metrics_pland <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_pland(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_pland[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
pland_metrics <- bind_rows(landscape_metrics_pland)
pland_metrics

# Create the stacked bar plot
ggplot(pland_metrics, aes(x = as.numeric(year), y = value, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percent land Cover",
       x = "Year",
       y = "Percent of Land covered by class",
       fill = "Class") +
  scale_fill_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()



# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
# Initialize a list to store landscape metrics for each year
landscape_metrics_coh <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_cohesion(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_coh[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
coh_metrics <- bind_rows(landscape_metrics_coh)
print(coh_metrics, n =32)


class_labels <- c("0" = "Non-Ag", "1" = "Ag")
ggplot(coh_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Patch Cohesion Over Time",
       x = "Year",
       y = "Patch Cohesion",
       color = "Class") +
  scale_color_manual(values = c("blue", "green"), labels = class_labels) +
  theme_minimal()


###############################################################################
# Landscape summary of patch level metrics (using BL watershed all years)
# non ag vs major ag classes
###############################################################################
reclassified_raster <- ag_raster_90_maj

# check that raster is compatible with package
landscapemetrics::check_landscape()

# mean area of patches in each class
# Initialize a list to store landscape metrics for each year
landscape_metrics_list <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_area_mn(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_list[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
c_area_metrics <- bind_rows(landscape_metrics_list)
c_area_metrics

# Plot changes in mean patch area over time
library(ggplot2)
c_area_metrics_sub <- c_area_metrics %>%
  filter(class %in% c(1, 2, 3))
c_area_metrics_sub


class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(c_area_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time Ag Combo",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

class_labels <- c("1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(c_area_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Mean Patch Area Over Time Ag Combo",
       x = "Year",
       y = "Mean Patch Area in Hectares",
       color = "Class") +
  scale_color_manual(values = c("green", "red", "orange"), labels = class_labels) +
  theme_minimal()

# describes percentage of landscape belonging to each class, measure of
# composition
landscape_metrics_pland_1 <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_pland(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_pland_1[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
pland_metrics_1 <- bind_rows(landscape_metrics_pland_1)

# Create the stacked bar plot
class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(pland_metrics_1, aes(x = as.numeric(year), y = value, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percent land Cover",
       x = "Year",
       y = "Percent of Land covered by class",
       fill = "Class") +
  scale_fill_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

# cohesion of class, this is an aggregation metric that gives info about the
# configuration of the landscape, used to access if patches of the same class are
# located next to each other or appear in isolation
# 0 = if isolated, 100 if cohesion is present (units are percent)
# Initialize a list to store landscape metrics for each year
landscape_metrics_coh <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_cohesion(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_coh[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
coh_metrics <- bind_rows(landscape_metrics_coh)
print(coh_metrics, n =32)

class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(coh_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Patch Cohesion Over Time",
       x = "Year",
       y = "Patch Cohesion",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

# look at patch dominance index "largest patch index"
landscape_metrics_lpi <- list()

# Calculate landscape metrics for each layer (year)
for (i in 1:nlyr(reclassified_raster)) {
  raster_layer <- reclassified_raster[[i]]
  year <- names(cdl_all_years)[i]

  # Calculate landscape metrics
  landscape_metrics <- lsm_c_lpi(raster_layer)
  landscape_metrics$year <- year
  landscape_metrics_lpi[[i]] <- landscape_metrics
}

# Combine all metrics into a single data frame
lpi_metrics <- bind_rows(landscape_metrics_lpi)
lpi_metrics
lpi_metrics_sub <- lpi_metrics %>%
  filter(class %in% c(1, 2, 3))
lpi_metrics_sub

class_labels <- c("0" = "Non-Ag", "1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(lpi_metrics, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Largest Patch Size by Class",
       x = "Year",
       y = " Percent",
       color = "Class") +
  scale_color_manual(values = c("blue", "green", "red", "orange"), labels = class_labels) +
  theme_minimal()

class_labels <- c("1" = "Ag", "2" = "Major_Ag", "3" = "Alfalfa")
ggplot(lpi_metrics_sub, aes(x = as.numeric(year), y = value, color = as.factor(class))) +
  geom_line() +
  labs(title = "Largest Patch Size by Class",
       x = "Year",
       y = " Percent",
       color = "Class") +
  scale_color_manual(values = c("green", "red", "orange"), labels = class_labels) +
  theme_minimal()

