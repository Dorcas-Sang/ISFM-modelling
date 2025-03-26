####Study area map ####

# Load necessary packages
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

install.packages("RColorBrewer")
library(RColorBrewer)

# Load Africa map data
africa_map <- ne_countries(continent = "Africa", returnclass = "sf")

# Filter to highlight Ghana
ghana_map <- africa_map[africa_map$name == "Ghana", ]

# Create the inset map highlighting Ghana
inset_map <- ggplot() +
  geom_sf(data = africa_map, fill = "gray29", color = "white") +
  geom_sf(data = ghana_map, fill = "darkred", color = "darkred") +
  theme_void() +  # Remove all non-essential plot elements
  ggtitle("Africa") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))


# Load the map data for Ghana
ghana_district_map <- ne_states(country = "Ghana", returnclass = "sf")

# Filter to highlight the Northern Region
northern_region_map <- ghana_district_map[ghana_district_map$region == "Northern", ]

# Define coordinates for Savelugu, Tolon, and Mion (approximate centroids)
districts <- data.frame(
  district = c("Savelugu", "Tolon", "Mion"),
  lon = c(-0.8217, -1.0563, -0.8426),  # Longitudes
  lat = c(9.6246, 9.4296, 9.3708)      # Latitudes
)

# Create the main map of Ghana
ghana_main_map <- ggplot() +
  # Color each region in Ghana differently
  geom_sf(data = ghana_district_map, aes(fill = name), color = "grey3", size = 0.3) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(ghana_district_map$name)))) +
  # Highlight the Northern Region
  geom_sf(data = northern_region_map, fill = NA, color = "darkgreen", size = 0.5) +
  # Add points for the specific districts (Savelugu, Tolon, Mion)
  geom_point(data = districts, aes(x = lon, y = lat), color = "red", size = 1.5) +
  # Add labels for the specific districts
  geom_text(data = districts, aes(x = lon, y = lat, label = district),
            nudge_y = 0.15, size = 2, color = "red", fontface = "bold") +
  theme_minimal() +
  ggtitle("Ghana") +
  xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "none")  # Hide the legend to keep the focus on the regions


# Combine the maps using cowplot
combined_map <- ggdraw() +
  draw_plot(ghana_main_map) +
  draw_plot(inset_map, x = 0.65, y = 0.7, width = 0.3, height = 0.3)

# Display the combined map
print(combined_map)

#Save the map
ggsave("combined_ghana_map.png", plot = combined_map, width = 10, height = 8, dpi = 300)


