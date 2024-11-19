# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)

# Load Kenya shapefile
kenya <- st_read("ken_admbnda_adm1_iebc_20180607/ken_admbnda_adm1_iebc_20180607.shp")

# Example data: Counties with CDC presence
cdc_presence <- c("Busia", "Meru", "Nakuru", "Siaya", "Nairobi", "Mombasa") # Replace with actual counties

# Add a column to indicate CDC presence
kenya <- kenya %>%
  mutate(cdc_present = ifelse(ADM1_EN %in% cdc_presence, "Yes", "No"))

# Example data: Surveillance systems (replace with actual data)
surveillance_data <- read.csv("points_coordinates.csv") %>%
  mutate(
    county = ADM1_EN,
    lon = longitude,
    lat = latitude,
    system = platform
  )


# Remove rows with missing or invalid coordinates
surveillance_data <- surveillance_data %>%
  filter(!is.na(lon) & !is.na(lat))

# Convert surveillance_data to spatial points and align CRS with Kenya shapefile
surveillance_sf <- st_as_sf(
  surveillance_data, 
  coords = c("lon", "lat"), 
  crs = st_crs(kenya)
)

# Plot the map
ggplot() +
  geom_sf(data = kenya, aes(fill = cdc_present), color = "black", size = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "yellow", "No" = "gray")) +
  geom_sf(data = surveillance_sf, aes(color = system), size = 3) +
  scale_color_manual(values = c("IFBS" = "blue", 
                                "SARI/ILI" = "red", 
                                "PBIDS" = "green", 
                                "AMR" = "purple")) +
  labs(
    title = "Kenya Map Showing CDC Presence and Surveillance Systems",
    color = "Surveillance System"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom"
  )



# Plot the map
overall <- ggplot() +
  geom_sf(data = kenya, aes(fill = cdc_present), color = "black", size = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "yellow", "No" = "gray")) +
  geom_sf(data = surveillance_sf, aes(color = system), size = 2) +
  scale_color_manual(values = c("IFBS" = "blue", 
                                "SARI/ILI" = "red", 
                                "PBIDS" = "green", 
                                "AMR" = "purple")) +
  labs(
    #title = "Kenya Map Showing CDC Presence and Surveillance Systems",
    color = "Platform"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()
  )

overall


kenya_county <- kenya %>% filter(ADM1_EN %in% c("Nakuru"))
surveillance_sf_county <- surveillance_sf %>% filter(county_name %in% c("Nakuru"))

county_centroids <- kenya_county %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2]
  )

# Plot the map
county <- ggplot() +
  geom_sf(data = kenya_county, aes(fill = cdc_present), color = "black", size = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "yellow", "No" = "gray")) +
  geom_sf(data = surveillance_sf_county, aes(color = system), size = 5) +
  scale_color_manual(values = c("IFBS" = "blue", 
                                "SARI/ILI" = "red", 
                                "PBIDS" = "green", 
                                "AMR" = "purple")) +
  geom_text(data = county_centroids, 
            aes(x = x, y = y, label = ADM1_EN),
            size = 5, color = "black", check_overlap = TRUE) +
  labs(
    #title = "Kenya Map Showing CDC Presence and Surveillance Systems",
    x = NULL,
    y = NULL,
    color = "Platform"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()
  )

county




