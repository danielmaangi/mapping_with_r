# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)

theme_set(
  theme_minimal(base_family = "sans") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
)

dark2_palette <- brewer.pal(8, "Dark2")

#1B9E77 (Teal Green)
#D95F02 (Orange)
#7570B3 (Purple)
#E7298A (Pink)
#66A61E (Green)
#E6AB02 (Yellow) 
#A6761D (Brown)
#666666 (Grey)

# Load Kenya shapefile
kenya <- st_read("ken_admbnda_adm1_iebc_20180607/ken_admbnda_adm1_iebc_20180607.shp")

waterbodies <- st_read("KEN_Lakes/KEN_Lakes.shp")

# Add a column to indicate CDC presence
kenya <- kenya %>%
  mutate(cdc_present = ifelse(ADM1_EN %in% cdc_presence, "Yes", "No"))

# Example data: Surveillance systems (replace with actual data)
prevalence_data <- read.csv("hiv_prevalence_data.csv") %>%
  filter(Level == "County") %>%
  transmute(
    county = case_when(County == "Nairobi (County)" ~ "Nairobi",
                       County == "Taita-Taveta" ~ "Taita Taveta",
                       County == "Trans-Nzoia" ~ "Trans Nzoia",
                       TRUE ~ County,
                       ),
    hiv_prevalence_all_sexes = as.numeric(gsub("%", "", `HIV.Prevelance.All.sexes.`))
  )

kenya <- kenya %>%
  left_join(prevalence_data, by = c("ADM1_EN" = "county"))
# Plot the map colored by HIV prevalence
overall <- ggplot() +
  geom_sf(data = kenya, aes(fill = hiv_prevalence_all_sexes), color = "black", size = 0.2) +
  geom_sf(data = waterbodies, fill = "lightblue", color = NA) +
  scale_fill_gradient2(
    name = "HIV Prevalence (%)",
    low = "#1B9E77",  # Teal Green
    midpoint = median(kenya$hiv_prevalence_all_sexes, na.rm = TRUE),
    high = "#D95F02",  # Bright Orange
    na.value = "#666666"  # Grey for NA
  ) +
  theme_minimal() +
  labs(
    title = "HIV Prevalence in Kenya",
    caption = "Source: HIV Estimates, 2023",
    y = element_blank(),
    x = element_blank()
  ) +
  annotate(
    geom = "text", x = 37.5, y = 6.5, label = "Overall Prevalence: 3.31%",
    color = "darkblue", fontface = "bold", size = 6
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()
  )

# Display the plot
print(overall)


overall


