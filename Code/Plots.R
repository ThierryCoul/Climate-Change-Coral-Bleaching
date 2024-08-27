# Importing the packages -----------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(rnaturalearth)
library(colorspace)
library(grid)
library(foreign)
library(prismatic)
library(ggpubr)


setwd("Set working directory to the Output.")

# Setting global variables for the figures
axis_text_size <- 10
axis_title_size <- 16
output_export_width <- 10
output_export_height <- 10
strip_text_size <- 16
legend_key_width <- 3
legend_text_size <- 15
legend_title_size <- 20

# grab country outlines / ocean areas as sf object
countries <- ne_countries(returnclass = "sf", scale = 50)


# Defining the functions
Importing_rasters <- function(name) { 
  raster_data <- raster(name)
  names(raster_data) <- c(name)
  return (raster_data)
}

# Plotting the coral location and their bleaching ------------------------------
# Importing the data
coral_location <- read_csv("../Input/data_Present_Future_dark_Spot.csv")

# making a histogram bleaching occurrence
png("Average_Bleaching.png", width = 1200, height = 1000)

# histogram with added parameters
hist(coral_location$Average_bleaching,
     main="Average Bleaching",
     xlab="Percentage",
     #xlim=c(50,100),
     col="grey90",
     freq=FALSE,
     cex.main=3,      # Increase title size
     cex.lab=1.7,       # Increase axis label size
     cex.axis=1.7       # Increase axis tick mark label size
)
meanvalue = mean(coral_location$Average_bleaching)
abline(v=meanvalue,col="grey10", lwd=3)
text(x = meanvalue, 
     y = par("usr")[4] * 0.95,
     labels = paste("Mean =", round(meanvalue, 2)),
     col = "grey10",
     pos = 4,
     cex = 2  # Increase the size of the text label
)  
dev.off()

my_sf <- st_as_sf(coral_location, coords = c('Longitude.Degrees', 'Latitude.Degrees'))
my_sf <- st_set_crs(my_sf, 4326)

my_sf <- my_sf %>%
  mutate(Bleaching = cut(Average_bleaching, breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                         labels = c("1-10", "11-20", "21-30", "30-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")))

my_sf_filtered_bleached <- my_sf %>% 
  filter(!is.na(Bleaching))

my_sf_filtered_bleached <- my_sf_filtered_bleached[order(my_sf_filtered_bleached$Bleaching, decreasing = FALSE),]

my_sf_filtered_non_bleached <- my_sf %>% 
  filter(is.na(Bleaching))

# Plot
graph <- ggplot() + 
  geom_sf(data = countries, color = "white", fill = "grey90", size = 3) +
  coord_sf(ylim = c(-30, 30)) +
  coord_equal() +
  theme_classic() +
  geom_sf(data = my_sf_filtered_non_bleached, aes(fill = NA), shape = 21, size = 2, stroke = 0.5, show.legend = FALSE) +
  geom_sf(data = my_sf_filtered_bleached, aes(fill = Bleaching), shape = 21, size = 2, stroke = 0) +
  scale_fill_viridis_d(option = "viridis") +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()   
  ) +
  guides(fill=guide_legend(title="Bleaching\npercentage")) +
  coord_sf(ylim = c(-40, 40))

# Check the plot
graph

# Export
ggsave("Survey location.png", graph, width = output_export_width, height = output_export_height, 
       dpi = 600, type = "cairo-png")

# Prediction from GWR --------------------------------------------------------
# Set the dictionary
BL_205045 <- "BL_205045_masked.tif"
BL_210045 <- "BL_210045_masked.tif"
BL_205085 <- "BL_205085_masked.tif"
BL_210085 <- "BL_210085_masked.tif"

BL_205045_tif <- Importing_rasters(BL_205045)
BL_205045_df <- as.data.frame(BL_205045_tif, xy = TRUE) %>% 
  na.omit()
BL_205085_tif <- Importing_rasters(BL_205085)
BL_205085_df <- as.data.frame(BL_205085_tif, xy = TRUE) %>% 
  na.omit()
BL_210045_tif <- Importing_rasters(BL_210045)
BL_210045_df <- as.data.frame(BL_210045_tif, xy = TRUE) %>% 
  na.omit()
BL_210085_tif <- Importing_rasters(BL_210085)
BL_210085_df <- as.data.frame(BL_210085_tif, xy = TRUE) %>% 
  na.omit()

# Merge the dataframes
df <- merge(BL_205045_df, BL_205085_df, by=c('x','y')) %>% 
  merge(BL_210045_df, by=c('x','y')) %>% 
  merge(BL_210085_df, by=c('x','y'))

# Reshape the data from wide to long
long_df <- 
  df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "RCPs",
    values_to = "Bleaching"
  ) %>%  
  mutate(RCPs = case_when(
    RCPs == 'BL_205045_masked.tif' ~ 'RCP 4.5 (2050)',
    RCPs == 'BL_205085_masked.tif' ~ 'RCP 8.5 (2050)',
    RCPs == 'BL_210045_masked.tif' ~ 'RCP 4.5 (2100)',
    RCPs == 'BL_210085_masked.tif' ~ 'RCP 8.5 (2100)',
    TRUE ~ RCPs 
  ))

# Transform the data to estimate the improvement of coral bleaching based on scenarios
wide_df <- long_df %>%
  group_by(x, y) %>%
  pivot_wider(names_from = RCPs, values_from = Bleaching)

difference_df <- wide_df %>%
  mutate(diff_2050_from_4_to_8 = `RCP 8.5 (2050)` - `RCP 4.5 (2050)`,
         diff_2100_from_4_to_8 = `RCP 8.5 (2100)` - `RCP 4.5 (2100)`) %>%
  ungroup() %>%
  dplyr::select(x, y, diff_2050_from_4_to_8, diff_2100_from_4_to_8)

# Reshape the dataframe from wide to long
improvement_df <- 
  difference_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "RCPs",
    values_to = "Bleaching"
  ) %>% 
  mutate(RCPs = case_when(
    RCPs == 'diff_2050_from_4_to_8' ~ 'Variations from RCP 4.5 to RCP 8.5 (2050)',
    RCPs == 'diff_2100_from_4_to_8' ~ 'Variations from RCP 4.5 to RCP 8.5 (2100)',
    RCPs == 'diff_SSP5_SSP2' ~ 'Variations from SSP2 to SSP5 (2021 - 2040)',
    TRUE ~ RCPs 
  ))

long_df_GWR <- long_df

## Graphs for all RCPs ---------------------------------------------------------
# Ploting the data
graph <- ggplot() +
  geom_raster(data = long_df, aes(x = x, y = y, fill = Bleaching)) +
  facet_wrap( ~ RCPs , ncol= 1) +
  scale_fill_viridis_b(option="cividis" , breaks = scales::pretty_breaks(n = 10)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),      
    legend.title = element_text(size = legend_title_size),     
    legend.key.width  = unit(legend_key_width, "cm"),          
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) +
  coord_sf(ylim = c(-30, 30))

# Check the plot
graph

# Export
ggsave("RCPs.png", graph, width = output_export_width, height = output_export_height, 
       dpi = 600, type = "cairo-png")


## Graphs Improvements ---------------------------------------------------------
# Ploting the data
diverging_colors <- c("#08519c","#3182bd","#9ecae1","grey98","#fcae91","#fb6a4a","#a50f15")
breaks <- c(-100, -50, -25, -1, 1, 25, 50, 100)
graph <- ggplot() +
  geom_raster(data = improvement_df, aes(x = x, y = y, fill = Bleaching)) +
  facet_wrap( ~ RCPs , ncol= 1) +
  scale_fill_stepsn(colors = diverging_colors, breaks = breaks, limits = range(breaks)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),      
    legend.title = element_text(size = legend_title_size),     
    legend.key.width  = unit(legend_key_width, "cm"),          
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) +
  coord_sf(ylim = c(-30, 30))

# Check the plot
graph

# Export
ggsave("Improvements_across_RCPs.png", graph, width = output_export_width, height = output_export_height, 
       dpi = 600, type = "cairo-png")

# Prediction from Stata --------------------------------------------------------
BL_205045 <- "Projection_RCP452050_station.tif"
BL_210045 <- "Projection_RCP852050_station.tif"
BL_205085 <- "Projection_RCP452100_station.tif"
BL_210085 <- "Projection_RCP852100_station.tif"

BL_205045_tif <- Importing_rasters(BL_205045)
BL_205045_df <- as.data.frame(BL_205045_tif, xy = TRUE) %>% 
  na.omit()
BL_205085_tif <- Importing_rasters(BL_205085)
BL_205085_df <- as.data.frame(BL_205085_tif, xy = TRUE) %>% 
  na.omit()
BL_210045_tif <- Importing_rasters(BL_210045)
BL_210045_df <- as.data.frame(BL_210045_tif, xy = TRUE) %>% 
  na.omit()
BL_210085_tif <- Importing_rasters(BL_210085)
BL_210085_df <- as.data.frame(BL_210085_tif, xy = TRUE) %>% 
  na.omit()

# Merge the dataframes
df <- merge(BL_205045_df, BL_205085_df, by=c('x','y')) %>% 
  merge(BL_210045_df, by=c('x','y')) %>% 
  merge(BL_210085_df, by=c('x','y'))

# Reshape the data from wide to long
long_df <- 
  df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "RCPs",
    values_to = "Bleaching"
  ) %>%  
  mutate(RCPs = case_when(
    RCPs == 'Projection_RCP452050_station.tif' ~ 'RCP 4.5 (2050)',
    RCPs == 'Projection_RCP852050_station.tif' ~ 'RCP 8.5 (2050)',
    RCPs == 'Projection_RCP452100_station.tif' ~ 'RCP 4.5 (2100)',
    RCPs == 'Projection_RCP852100_station.tif' ~ 'RCP 8.5 (2100)',
    TRUE ~ RCPs 
  ))

# Transform the data to estimate the improvement of coral bleaching based on scenarios
wide_df <- long_df %>%
  group_by(x, y) %>%
  pivot_wider(names_from = RCPs, values_from = Bleaching)

difference_df <- wide_df %>%
  mutate(diff_2050_from_4_to_8 = `RCP 8.5 (2050)` - `RCP 4.5 (2050)`,
         diff_2100_from_4_to_8 = `RCP 8.5 (2100)` - `RCP 4.5 (2100)`) %>%
  ungroup() %>%
  dplyr::select(x, y, diff_2050_from_4_to_8, diff_2100_from_4_to_8)

# Reshape the dataframe from wide to long
improvement_df <- 
  difference_df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "RCPs",
    values_to = "Bleaching"
  ) %>% 
  mutate(RCPs = case_when(
    RCPs == 'diff_2050_from_4_to_8' ~ 'Variations from RCP 4.5 to RCP 8.5 (2050)',
    RCPs == 'diff_2100_from_4_to_8' ~ 'Variations from RCP 4.5 to RCP 8.5 (2100)',
    RCPs == 'diff_SSP5_SSP2' ~ 'Variations from SSP2 to SSP5 (2021 - 2040)',
    TRUE ~ RCPs 
  ))

## Graphs for all RCPs ---------------------------------------------------------
# Ploting the data
graph <- ggplot() +
  geom_raster(data = long_df, aes(x = x, y = y, fill = Bleaching)) +
  facet_wrap( ~ RCPs , ncol= 1) +
  scale_fill_viridis_b(option="cividis" , breaks = scales::pretty_breaks(n = 10)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),      
    legend.title = element_text(size = legend_title_size),     
    legend.key.width  = unit(legend_key_width, "cm"),          
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) +
  coord_sf(ylim = c(-30, 30))

# Check the plot
graph

# Export
ggsave("RCPs_station.png", graph, width = output_export_width, height = output_export_height, 
       dpi = 600, type = "cairo-png")

## Graphs Improvements ---------------------------------------------------------
# Ploting the data
diverging_colors <- c("#08519c","#3182bd","#9ecae1","grey98","#fcae91","#fb6a4a","#a50f15")
breaks <- c(-100, -50, -25, -1, 1, 25, 50, 100)
graph <- ggplot() +
  geom_raster(data = improvement_df, aes(x = x, y = y, fill = Bleaching)) +
  facet_wrap( ~ RCPs , ncol= 1) +
  scale_fill_stepsn(colors = diverging_colors, breaks = breaks, limits = range(breaks)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),      
    legend.title = element_text(size = legend_title_size),     
    legend.key.width  = unit(legend_key_width, "cm"),          
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) +
  coord_sf(ylim = c(-30, 30))

# Check the plot
graph

# Export
ggsave("Improvements_across_RCPs_station.png", graph, width = output_export_width, height = output_export_height, 
       dpi = 600, type = "cairo-png")

# Comparison between prections of GWR and stationnary Poisson regression ------
# Transforming the labels for the stationry observations 
long_df_for_plot <- long_df %>%
  mutate(RCPs = paste(RCPs, " stationnary"))

# Transforming the labels for the non-stationry observations 
long_df_GWR_for_plot <- long_df_GWR %>%
  mutate(RCPs = paste(RCPs, " non-stationnary"))

# Appending data 
total_long_df <- rbind(long_df_GWR_for_plot, long_df_for_plot)
total_long_df <- total_long_df[order(total_long_df$x, total_long_df$y, total_long_df$RCPs), ]

## Graphs for all RCPs ---------------------------------------------------------
# Ploting the data
graph <- ggplot() +
  geom_raster(data = total_long_df, aes(x = x, y = y, fill = Bleaching)) +
  facet_wrap( ~ RCPs , ncol= 2) +
  scale_fill_viridis_b(option="cividis" , breaks = scales::pretty_breaks(n = 10)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = legend_text_size),      
    legend.title = element_text(size = legend_title_size),     
    legend.key.width  = unit(legend_key_width, "cm"),          
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) + 
  coord_sf(ylim = c(-30, 30)) +  
  geom_text(
    data = subset(total_long_df, RCPs == "RCP 4.5 (2050)  stationnary"), 
    aes(x = 50, y = -25, label = "Madagascar"), 
    size = 3, fontface = "bold"
  ) +  
  geom_text(
    data = subset(total_long_df, RCPs == "RCP 4.5 (2050)  non-stationnary"), 
    aes(x = 130, y = -10, label = "Great Barrier Reef\n& South Asia"), 
    size = 2, fontface = "bold"
  )

# Check the plot
graph

# Export
ggsave("RCPs_Comparison.png", graph, width = output_export_width*2, height = output_export_height, 
       dpi = 600, type = "cairo-png")

# Plot the GWR coefficients without control variables  ------------------------
# Import the coefficients files
Temp_coef <- Importing_rasters("GWR_MEAN_SST_ressampled.tif")
TSA_coef <- raster("GWR_MEAN_TSA_ressampled.tif")

Temp_coef_df <- as.data.frame(Temp_coef, xy = TRUE) %>% 
  na.omit()
TSA_coef_df <- as.data.frame(TSA_coef, xy = TRUE) %>% 
  na.omit()

png("Coefficent_SSTA_hist.png", width = 1200, height = 1000)
  
# histogram with added parameters
hist(TSA_coef_df$Band_1,
     main="Coefficient of SSTA on coral bleaching",
     xlab="Coefficients",
     #xlim=c(50,100),
     col="grey90",
     freq=FALSE,
     cex.main=3,      # Increase title size
     cex.lab=1.7,       # Increase axis label size
     cex.axis=1.7       # Increase axis tick mark label size
)
meanvalue = mean(TSA_coef_df$Band_1)
abline(v=meanvalue,col="grey10", lwd=3)
text(x = meanvalue, 
     y = par("usr")[4] * 0.95,
     labels = paste("Mean =", round(meanvalue, 2)),
     col = "grey10",
     pos = 4,
     cex = 2  # Increase the size of the text label
     )  

dev.off()

png("Coefficent_SST_hist.png", width = 1200, height = 1000)
# histogram with added parameters
hist(Temp_coef_df$GWR_MEAN_SST_ressampled.tif,
     main="Coefficient of SST on coral bleaching",
     xlab="Coefficients",
     #xlim=c(50,100),
     col="grey90",
     freq=FALSE,
     cex.main=3,      # Increase title size
     cex.lab=1.7,       # Increase axis label size
     cex.axis=1.7       # Increase axis tick mark label size
)
meanvalue = mean(Temp_coef_df$GWR_MEAN_SST_ressampled.tif)
abline(v=meanvalue,col="grey10", lwd=3)
text(x = meanvalue, 
     y = par("usr")[4] * 0.95, 
     labels = paste("Mean =", round(meanvalue, 2)),
     col = "grey10",
     pos = 4,
     cex = 2  # Increase the size of the text label
     ) 
dev.off()

# Merge the dataframes
df <- merge(Temp_coef_df, TSA_coef_df, by=c('x','y')) %>% 
  dplyr::select("x","y","GWR_MEAN_SST_ressampled.tif", "Band_1")

# Reshape the data from wide to long
long_df <- 
  df %>%
  pivot_longer(
    c(-x, -y),
    names_to = "Variables",
    values_to = "Coefficients"
  )

# Replace the title of the graphs
long_df <- long_df %>% 
  mutate(Variables = case_when(
    Variables == 'GWR_MEAN_SST_ressampled.tif' ~ 'Impacts of sea surface temperature(SST) mean',
    Variables == 'Band_1' ~ 'Impacts of sea surface temperature anomalies (SSTA)',
    TRUE ~ Variables 
  )) 

diverging_colors <- c("#08519c","#3182bd","#6baed6","#bdd7e7","#eff3ff","#fee5d9","#fcae91","#fb6a4a","#de2d26","#a50f15")
breaks <- c(-2.5, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)

long_df <- long_df[order(long_df$x, long_df$y, long_df$Variables), ]

# Ploting the data
graph <- ggplot() +
  geom_raster(data = long_df, aes(x = x, y = y, fill = Coefficients)) +
  facet_wrap( ~ Variables , ncol= 1) +
  #scale_fill_viridis_b(option="cividis" , breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_stepsn(colors = diverging_colors, breaks = breaks, limits = range(breaks)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),      
    legend.title = element_text(size = legend_title_size),
    legend.key.width  = unit(legend_key_width, "cm"), 
    panel.border = element_rect(colour = "black", fill=NA),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    strip.text = element_text(size = strip_text_size)
  ) +
  coord_equal() +
  geom_sf(data = countries,
          color = "white",
          fill = "grey90",
          size = 3) +
  coord_sf(ylim = c(-30, 30)) +
  geom_text(
    data = subset(long_df, Variables == "Impacts of sea surface temperature anomalies (SSTA)"), 
    aes(x = 50, y = -12, label = "Southern\nAfrica"), 
    size = 3, fontface = "bold"
  ) +
  geom_text(
    data = subset(long_df, Variables == "Impacts of sea surface temperature anomalies (SSTA)"), 
    aes(x = 130, y = 12, label = "South East\nAsia"), 
    size = 3, fontface = "bold"
  ) +
  geom_text(
    data = subset(long_df, Variables == "Impacts of sea surface temperature anomalies (SSTA)"), 
    aes(x = -140, y = 0, label = "South Pacific"), 
    size = 3, fontface = "bold"
  ) +
  geom_text(
    data = subset(long_df, Variables == "Impacts of sea surface temperature anomalies (SSTA)"), 
    aes(x = 85, y = -2, label = "Indonesia\ncoast"), 
    size = 3, fontface = "bold"
  )

# Check the plot
graph

# Export
ggsave("Coefficients.png", graph, width = output_export_width, height = output_export_height/2, 
       dpi = 600, type = "cairo-png")

# Design the function for the prediction by eco-region -------------------------
graphs_bleaching_function <- function(file, ytitle, plotname, y_offset) {
  dbf <- read.dbf(file, as.is = F)

  dbf$MEAN <- dbf$MEAN /100
  dbf$MIN <- dbf$MIN /100
  dbf$MAX <- dbf$MAX /100
  
  # Calculate the mean of each numeric column
  min_min <- min(dbf$MIN)
  mean_mean <- sum(dbf$SUM) / sum(dbf$AREA) / 100
  max_max <- max(dbf$MAX)
  new_row <- data.frame(Ecoregion = "Summary of ecoregions", MIN = min_min, MAX = max_max, MEAN = mean_mean)
  
  dbf <- dbf %>% dplyr::select(Ecoregion, MIN, MAX, MEAN)
  
  dbf <- dbf %>%
    arrange(MEAN)
  head <- head(dbf, 10)
  tail <- tail(dbf, 10)
  dbf <- rbind(head, tail, new_row)
  
  dbf <- dbf %>%
    arrange(MEAN)
  
  first_value <- dbf$Ecoregion[1]
  third_value <- dbf$Ecoregion[5]
  fifth_value <- dbf$Ecoregion[10]
  sixth_value <- dbf$Ecoregion[12]
  eight_value <- dbf$Ecoregion[15]
  tenth_value <- dbf$Ecoregion[21]
  
  writting_offset <- y_offset * 1.1 
  
  Legend_key <- 'Average bleaching'
  variations <- 'Average bleaching'
  coral_bleached_color <- rgb(0.7843137, 0,0)
  size_label <- 3
  
  p0 <- ggplot(dbf, aes(x= fct_reorder(Ecoregion, MEAN), y=MEAN, fill = after_scale(clr_negate(color)), color = MEAN)) +
    geom_col(alpha=0.9, width = 0.7) +
    geom_errorbar(aes(ymin = MIN, ymax = MAX),color= "grey90" ,
                  position = position_dodge(0.9), width = .3) +
    scale_fill_manual("legend",
                      values = coral_bleached_color,
                      breaks = variations,
                      labels= Legend_key) +
    labs(x = "Ecoregion", y = ytitle, color = "Key")  +
    geom_text(aes(x= Ecoregion, y = abs(MEAN) + 0.05, label = scales::percent(round(MEAN,3))), size=size_label , color = "black") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.title=element_blank(),
          legend.text = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=axis_text_size,face="bold"),
          axis.title=element_text(size=axis_title_size,face="bold"),
          legend.position = "None")+
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    annotate("text", x = third_value, y = writting_offset, label = "Bottom 10 bleaching proportions", angle='270', size = axis_text_size/1.5) +
    annotate("text", x = eight_value, y = writting_offset, label = "Top 10 Bleaching proportions", angle='270', size = axis_text_size/1.5)
  
  # Saving the plot
  ggsave(plotname, last_plot(), width = output_export_width, height = output_export_height,
         dpi = 600, type = "cairo-png", bg="white")
  
return (p0)
}

## Prediction 2050 (RCP 4.5) with GWR ------------------------------------------
graphs_bleaching_function("Projections_BL_205045.dbf",
                          "Predictions of bleaching proportion in 2050 (RCP 4.5)",
                          "Bleaching_2050_RCP45.png",
                          1.1)