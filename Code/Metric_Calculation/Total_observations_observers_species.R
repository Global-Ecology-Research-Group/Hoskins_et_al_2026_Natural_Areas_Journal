# Get total observations, observers, and species at the site and relative to the region

library(tidyverse)
library(scales)
library(sf)
library(biscale)
library(maptiles)
library(terra)
library(ggspatial)

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# remove the one day with 3 observations
unique(deluca_bioblitz$observed_on) #5, but 2023 is broken into the 24th and 25th (3 observations)
sum(deluca_bioblitz$observed_on == "2023-02-25")
deluca_bioblitz <- deluca_bioblitz %>%
  filter(observed_on != "2023-02-25")  %>%
  mutate(year = year(observed_on)) # add year column

# read in all iNaturalist data in Florida
inat_files <- list.files("Data/Florida_Data/", full.names = TRUE)
inat_combined <- bind_rows(lapply(inat_files[1:65], read_csv))

# Select relevant columns
inat_combined_research <- inat_combined %>%
  dplyr::select(
    id, uuid, observed_on, time_observed_at, created_at,
    quality_grade, license, url, image_url,
    user_id, user_login,
    place_guess, latitude, longitude, positional_accuracy,
    place_town_name, place_county_name, place_state_name,
    place_country_name,
    species_guess, scientific_name, common_name, iconic_taxon_name,
    taxon_id, 
    taxon_kingdom_name, taxon_phylum_name, taxon_class_name,
    taxon_order_name, taxon_family_name, taxon_genus_name,
    taxon_species_name, taxon_subspecies_name
  )

# Site-Level ------------------------------------------------------

## Total observations ------------------------------------------------------

# total observations at the bioblitz
nrow(deluca_bioblitz)

### Map of observations at the site ----------------------

#### Map of Study Location ----------------------------------------------------------

# Make figure showing Osceola county's position in Florida

# Load all Florida counties
fl_counties <- counties(state = "FL", cb = TRUE, class = "sf")

# Identify Osceola County
highlight_counties <- fl_counties %>%
  filter(NAME %in% c("Osceola"))

# Create a new column for fill color
fl_counties <- fl_counties %>%
  mutate(fill_color = ifelse(NAME %in% c("Osceola"), "darkred", "white"))

# Plot
ggplot() +
  geom_sf(data = fl_counties, aes(fill = fill_color), color = "black", size = 0.3) +
  scale_fill_identity() +  
  theme_void() +
  theme(
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Save as PNG
ggsave(filename = "Figures/Figure_2_osceola_county_map.png", width = 8, height = 6, dpi = 300)

#### Bivariate classification and mapping ----------------------------------------------------------

# read in shapefile of DeLuca
deluca <- st_read("Data/Shapefile/deluca.shp")

# Transform DeLuca to WGS84
deluca <- st_transform(deluca, crs = 4326)

# Convert bioblitz data to sf
bioblitz_sf <- deluca_bioblitz %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Keep only points within DeLuca
bioblitz_sf <- st_filter(bioblitz_sf, deluca, .predicate = st_within)

# Project to UTM zone
bioblitz_sf_proj <- st_transform(bioblitz_sf, 32617)

# Create 500m hex grid for data
hex_grid <- st_make_grid(bioblitz_sf_proj, cellsize = 500, square = FALSE) %>%
  st_sf() %>%
  mutate(hex_id = row_number())

# Join points to hex grid
bioblitz_joined <- st_join(bioblitz_sf_proj, hex_grid, join = st_within)

# Summarize observation and species counts per hex
hex_summary <- bioblitz_joined %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  summarise(
    n_obs = n(),
    n_species = n_distinct(taxon_species_name)
  ) %>%
  left_join(hex_grid, by = "hex_id") %>%
  st_as_sf()

# Transform back to lon/lat for plotting
hex_summary_ll <- st_transform(hex_summary, 4326)

# Map DeLuca, add bivariate plot

# Create bivariate classification
# Adjust 'style' or 'dim' as needed
hex_bi <- bi_class(
  hex_summary_ll,
  x = n_obs,
  y = n_species,
  style = "quantile",
  dim = 3
)

# Create an expanded bbox around your hex layer
bbox_orig <- st_bbox(hex_bi) 
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom = 15, provider = "Esri.WorldImagery", crop = TRUE)

# Get the bounding box of bioblitz site
deluca_bbox <- st_bbox(deluca)

# Crop to bounding box first, then mask to exact shape
sat_cropped <- crop(sat_map, deluca)
sat_masked  <- mask(sat_cropped, deluca)
plot(sat_masked)

# Convert the raster to a data frame
sat_masked_df <- as.data.frame(sat_masked, xy = TRUE)

# Rename the columns for clarity (x, y, R, G, B)
names(sat_masked_df) <- c("x", "y", "R", "G", "B")

# Then plot using ggRGB on sat_masked 
bi_map_sat <- ggplot() +
  geom_raster(data = sat_masked_df, aes(x = x, y = y), fill = rgb(sat_masked_df$R, sat_masked_df$G, sat_masked_df$B, maxColorValue = 255)) +
  geom_sf(data = hex_bi, aes(fill = bi_class), color = "black", size = 0.3, alpha = 1) +
  geom_sf(data = deluca, fill = NA, color = "black", size = 1) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  annotation_scale(location = "bl", width_hint = 0.3,
                   pad_x = unit(0.2, "in"), pad_y = unit(0.1, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering()) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = expression("Longitude ("*degree*W*")"),
    y = expression("Latitude ("*degree*N*")")
  ) +
  coord_sf(
    xlim = c(deluca_bbox["xmin"], deluca_bbox["xmax"]),
    ylim = c(deluca_bbox["ymin"], deluca_bbox["ymax"])
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

bi_map_sat

ggsave("Figures/Figure_2_map.png", plot = bi_map_sat, bg = "transparent")

# Bivariate legend
bi_legend <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "Higher Observation Density →",
  ylab = "↑ Higher Species Richness",
  size = 8
)
bi_legend

ggsave("Figures/Figure_2_bivariate_legend_deluca.png", plot = bi_legend, bg = "transparent")



## Total Observers ------------------------------------------------------

# Get number of unique observers at the bioblitz
length(unique(deluca_bioblitz$user_login))

### Observer trends by year --------------------------------------------------------------

# Get unique observers per year
observers_by_year <- deluca_bioblitz %>%
  group_by(year) %>%
  summarise(observers = list(unique(user_login)), .groups = "drop")

# Build cumulative list of all past observers BEFORE each year
previous_all <- accumulate(observers_by_year$observers, union) |> lag()

# Attach cumulative history and calculate repeat rate
repeat_observer_rates <- observers_by_year %>%
  mutate(
    previous_all = previous_all,
    n_total = lengths(observers),
    n_repeat = map2_int(observers, previous_all, ~ if (is.null(.y)) NA_integer_ else length(intersect(.x, .y))),
    repeat_rate = n_repeat / n_total
  )

# Repeat observer rates
repeat_observer_rates

# Calculate observers present in all years
observers_every_year <- reduce(observers_by_year$observers, intersect)
length(observers_every_year)

### Distribution of number of observations per observer ---------------------

# Plot figure for observations and observers bell curve log scale

# Summarize number of observations per observer
observer_summary <- deluca_bioblitz %>%
  filter(!is.na(user_login)) %>%
  group_by(user_login) %>%
  summarise(n_obs = n()) %>%
  ungroup()

# Plot (log x-axis for visualization)
## Add flag for top 5% observers
observer_summary <- observer_summary %>%
  mutate(top_5 = n_obs >= quantile(n_obs, 0.95))

# Plot with highlighted top 5%
o_productivity <- ggplot(observer_summary, aes(x = n_obs, fill = top_5)) +
  geom_histogram(
    bins = 30,
    color = "white",
    alpha = 0.9,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "grey50", "TRUE" = "darkolivegreen3"),
    labels = c("Other Observers", "Top 5% Observers"),
    name = NULL
  ) +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000),
    labels = comma
  ) +
  labs(
    title = NULL,
    x = "Number of Observations (log scale)",
    y = "Number of Observers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

o_productivity

## Total species ------------------------------------------------------

# Filter to only include research grade data
deluca_bioblitz_research <- deluca_bioblitz %>%
  filter(quality_grade=="research")

# Get a list of species
species <- unique(deluca_bioblitz_research$taxon_species_name)

# Total unique species overall
total_species <- deluca_bioblitz %>%
  summarise(n_species = n_distinct(taxon_species_name)) %>%
  pull(n_species)
total_species

# Total unique species in research grade only
research_species <- deluca_bioblitz %>%
  filter(quality_grade == "research") %>%
  summarise(n_species = n_distinct(taxon_species_name)) %>%
  pull(n_species)
research_species

### Summarize top species and recording frequency --------------------------------------------------------------

# Count the number of observations per species
most_observed_species <- deluca_bioblitz_research %>%
  group_by(taxon_species_name) %>%
  summarise(observation_count = n()) %>%
  arrange(desc(observation_count))

# View top 10 most observed species
head(most_observed_species, 10)

# Count how many species have only 1 or 2 research-grade observations
rare_species_2 <- most_observed_species %>%
  filter(observation_count <= 2)
nrow(rare_species_2) 

# Count how many species have only 1 research-grade observation
rare_species_1 <- most_observed_species %>%
  filter(observation_count <= 1)
nrow(rare_species_1) 

# Quick visualization of top five species + number of observations
deluca_bioblitz_research %>%
  group_by(taxon_species_name, common_name) %>%
  summarise(observation_count = n(), .groups = "drop") %>%
  arrange(desc(observation_count)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(common_name, observation_count), 
             y = observation_count, 
             fill = common_name)) + 
  geom_col() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  coord_flip() +
  theme(legend.position = "none") 

# Regional-Level ------------------------------------------------------

# For Regional-Level comparisons, summaries are made for total observations, number of species,
# and number of observers concurrently

# total observations in the region
nrow(inat_combined_research)

# proportion of regional observations that were contributed during the bioblitz
nrow(deluca_bioblitz)/(nrow(inat_combined_research)-nrow(deluca_bioblitz))*100

# Get number of unique observers in the region
length(unique(inat_combined_research$user_login))

# Proportion of regional observers that were contributed during the bioblitz
length(unique(deluca_bioblitz$user_login))/(length(unique(c(inat_combined_research$user_login,deluca_bioblitz$user_login))))*100

# Number of species contributed to the region
length(unique(inat_combined_research$taxon_species_name))

# Proportion of regional species that were contributed during the bioblitz
length(unique(deluca_bioblitz[deluca_bioblitz$quality_grade=="research",]$taxon_species_name))/(length(unique(c(inat_combined_research$taxon_species_name,deluca_bioblitz[deluca_bioblitz$quality_grade=="research",]$taxon_species_name))))*100

## Comparison to random polygons -------------------------------------------

# Read in random polygons
random_polygon_effort <- read_csv("Data/Summarized_Data/random_polygon_effort.csv")

# read in shapefile of DeLuca
deluca <- st_read("Data/Shapefile/deluca.shp")

# Transform DeLuca to WGS84
deluca <- st_transform(deluca, crs = 4326)

# Get site area area in km²
deluca_area_km2 <- as.numeric(st_area(deluca)) / 1e6
deluca_area_km2

# Calculate site values per 1 km²
deluca_values_norm <- data.frame(
  Observers_per_km2     = length(unique(deluca_bioblitz$user_id)) / deluca_area_km2,
  Observations_per_km2  = nrow(deluca_bioblitz) / deluca_area_km2,
  Species_per_km2       = deluca_bioblitz %>%
    filter(quality_grade == "research", !is.na(taxon_species_name)) %>%
    distinct(taxon_species_name) %>%
    nrow() / deluca_area_km2) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = recode(Metric,
                    "Observers_per_km2"    = "Observers per km²",
                    "Observations_per_km2" = "Observations per km²",
                    "Species_per_km2"      = "Species per km²"
    ),
    Source = "DeLuca Bioblitz"
  )

# Calculate random polygon values per 1 km²
random_values_norm <- random_polygon_effort %>%
  dplyr::select(polygon_id, number_of_observations, number_of_observers, number_of_species) %>%
  mutate(
    Observers_per_km2    = number_of_observers / deluca_area_km2,
    Observations_per_km2 = number_of_observations / deluca_area_km2,
    Species_per_km2      = number_of_species / deluca_area_km2
  ) %>%
  dplyr::select(polygon_id, Observers_per_km2, Observations_per_km2, Species_per_km2) %>%
  pivot_longer(cols = c("Observers_per_km2","Observations_per_km2","Species_per_km2"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = recode(Metric,
                    "Observers_per_km2"    = "Observers per km²",
                    "Observations_per_km2" = "Observations per km²",
                    "Species_per_km2"      = "Species per km²"
    ),
    Source = "Random Polygons"
  )

# Combine datasets
combined_values <- bind_rows(deluca_values_norm, random_values_norm)

# Plot (violin + boxplot + DeLuca point overlay)
fig_4 <- combined_values %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_violin(
    data = subset(combined_values, Source == "Random Polygons"),
    fill = "gray80", color = "black", alpha = 0.6
  ) +
  geom_boxplot(
    data = subset(combined_values, Source == "Random Polygons"),
    width = 0.15, outlier.shape = NA, alpha = 0.8
  ) +
  geom_point(
    data = subset(combined_values, Source == "DeLuca Bioblitz"),
    color = "darkgoldenrod1",
    size = 4,
    shape = 18
  ) +
  scale_y_log10() + 
  labs(
    title = NULL,
    x = "",
    y = "Value (log-scaled)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )

fig_4

## Save as png
ggsave("Figures/Figure_4.png", plot = fig_4, bg = "transparent")

# Perform a one-sample t-test: compare DeLuca value to random polygon distribution
combined_values %>%
  group_by(Metric) %>%
  group_modify(~ {
    deluca_val <- .x %>% filter(Source == "DeLuca Bioblitz") %>% pull(Value)
    random_vals <- .x %>% filter(Source == "Random Polygons") %>% pull(Value)
    t_res <- t.test(random_vals, mu = deluca_val)
    tibble(
      Metric = unique(.x$Metric),
      DeLuca_Value = deluca_val,
      Random_Mean = mean(random_vals),
      Random_SD = sd(random_vals),
      t_statistic = t_res$statistic,
      df = t_res$parameter,
      p_value = t_res$p.value
    )
  }) %>%
  ungroup()
