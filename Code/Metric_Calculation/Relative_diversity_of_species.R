# Diversity of species at the site compared to the region

library(tidyverse)
library(scales)
library(vegan)

# Read in data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# Filter to only include research grade data
deluca_bioblitz_research <- deluca_bioblitz %>%
  filter(quality_grade=="research")

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

# Relative diversity of species -------------------------------------------

# Calculate Shannon's Diversity Index for the site
deluca_counts <- deluca_bioblitz_research %>%
  count(taxon_species_name) %>%
  pull(n)
shannon_deluca <- diversity(deluca_counts, index = "shannon")
shannon_deluca

# Calculate Shannon's Diversity Index for the region
fl_counts <- inat_combined_research %>%
  count(taxon_species_name) %>%
  pull(n)
shannon_florida <- diversity(fl_counts, index = "shannon")
shannon_florida

# Compute uniqueness indicators

# Precompute sets so takes less time to run for deluca, osceola, and florida
deluca_species <- deluca_bioblitz_research$taxon_species_name
osceola_species <- inat_combined_research %>%
  filter(place_county_name == "Osceola") %>%
  pull(taxon_species_name)
florida_species <- inat_combined_research$taxon_species_name

# Create summary table
records_value_summary <- tibble(
  taxon_species_name = unique(c(deluca_species, florida_species))
) %>%
  mutate(
    deluca_bioblitz = taxon_species_name %in% deluca_species,
    osceola_county = taxon_species_name %in% osceola_species,
    florida_state = taxon_species_name %in% florida_species
  )

# Summary metrics
summary_metrics <- records_value_summary %>%
  summarise(
    total_species = n(),
    deluca_unique_species = sum(deluca_bioblitz & !osceola_county & !florida_state),
    county_species = sum(osceola_county),
    pct_county = mean(osceola_county) * 100,
    state_species = sum(florida_state),
    pct_state = mean(florida_state) * 100
  )

summary_metrics

# visualize data

# Identify species unique to DeLuca
records_value_summary <- records_value_summary %>%
  mutate(unique_to_deluca = deluca_bioblitz & !osceola_county & !florida_state)

records_value_summary %>% 
  summarise(
    n_deluca = sum(deluca_bioblitz),
    n_osceola = sum(osceola_county),
    n_florida = sum(florida_state),
    n_unique = sum(unique_to_deluca)
  )

# Total number of Florida species
n_florida <- sum(records_value_summary$florida_state)

# Bar heights: all proportions relative to Florida
df_plot <- tibble(
  Region = factor(
    c("DeLuca", "Osceola", "Florida"),
    levels = c("DeLuca", "Osceola", "Florida")
  ),
  PropFlorida = c(
    sum(records_value_summary$deluca_bioblitz) / n_florida,
    sum(records_value_summary$osceola_county) / n_florida,
    n_florida / n_florida  # always 1
  )
)

# Plot
ggplot(df_plot, aes(x = Region, y = PropFlorida)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    y = "% of Florida Species Present",
    x = "Region",
    title = NULL
  ) +
  theme_minimal()

# plot by rarity classification
# Rarity classification for species in DeLuca Bioblitz
deluca_counts <- deluca_bioblitz_research %>%
  count(taxon_species_name, name = "obs_count")

deluca_counts %>%
  mutate(rarity_class = case_when(
    obs_count <= 5 ~ "Very rare (≤5)",
    obs_count <= 20 ~ "Rare (6–20)",
    TRUE ~ "Common (>20)"
  )) %>%
  count(rarity_class) %>%
  ggplot(aes(x = rarity_class, y = n, fill = rarity_class)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "DeLuca Rarity Class",
    y = "Number of Species Documented",
    title = NULL
  ) +
  theme_minimal() +
  coord_flip()

