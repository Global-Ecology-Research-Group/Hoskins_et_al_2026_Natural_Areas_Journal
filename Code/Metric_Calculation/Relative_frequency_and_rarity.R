# Frequency of species at the site relative to the region
# Measure of species rarity based on number of observations at the site and region

library(tidyverse)
library(scales)

# Read in data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# Filter to only include research grade data
deluca_bioblitz_research <- deluca_bioblitz %>%
  filter(quality_grade=="research")

# Read in total iNat data with just counts and remove nas
regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv") %>%
  dplyr::filter(!is.na(Species))

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

# Relative frequency ------------------------------------------------------

# Prepare relative observation ratio per species
# Count occurrences in DeLuca
deluca_freq <- deluca_bioblitz_research %>%
  count(taxon_species_name, name = "obs_deluca")

# Count occurrences in Florida (restricted to species observed at DeLuca)
inat_freq <- inat_combined_research %>%
  filter(taxon_species_name %in% deluca_freq$taxon_species_name) %>%
  count(taxon_species_name, name = "obs_florida")

# Combine data frames
freq_df <- deluca_freq %>%
  left_join(inat_freq, by = "taxon_species_name") 

# Compute bioblitz frequency relative to site
freq_df <- freq_df %>%
  mutate(deluca_freq = obs_deluca / obs_florida)

### Relative frequency at three spatial scales --------------------

# Supplemental figure 3

regional_counts <- regional_species_counts %>%
  mutate(
    Prop_Osceola = deluca_bioblitz / `Osceola County`,
    Prop_Florida = deluca_bioblitz / Florida,
    Prop_All = deluca_bioblitz / All
  ) %>%
  dplyr::select(Species, Prop_Osceola, Prop_Florida, Prop_All) %>%
  pivot_longer(
    cols = starts_with("Prop"),
    names_to = "Group",
    values_to = "Proportion"
  ) %>%
  mutate(
    Group = factor(Group, 
                   levels = c("Prop_Osceola", "Prop_Florida", "Prop_All"),
                   labels = c("Osceola County", "Florida", "All of iNaturalist"))
  )

# Plot
regional_counts_plot <- ggplot(regional_counts, aes(x = Group, y = Proportion)) +
  geom_boxplot(fill = "darkolivegreen3", alpha = 1, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black", size = 1.8) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10, sigma = 1e-4),
    breaks = c(0, 0.001, 0.01, 0.1, 0.5, 1),
    labels = c("0", "0.001", "0.01", "0.1", "0.5", "1"),
    limits = c(0, 1)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = NULL,
    x = "",
    y = "Proportion of Records from DeLuca"
  ) +
  theme(
    axis.text.x = element_text(color = "black", angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

regional_counts_plot

ggsave("Figures/Supplemental/Figure_S3.png", plot = regional_counts_plot, bg = "transparent")

# Relative Rarity ---------------------------------------------------------

# Define relative rarity based on observation counts at the site and region
relative_rarity_df <- freq_df %>%
    mutate(
    deluca_only = (obs_deluca == obs_florida),
    category = case_when(
      deluca_only ~ "Unique to DeLuca",
      obs_deluca < 10 & obs_florida > 25 ~ "Locally Rare",  
      obs_deluca > 10 & obs_florida > 25 ~ "Common Everywhere",
      obs_deluca < 10 & obs_florida <= 25 ~ "Rare Everywhere",
      TRUE ~ "Underreported Everywhere"
    )
  )

# Plot relative rarity
freq_plot_prop_clean <- ggplot(
  relative_rarity_df,
  aes(
    x = obs_deluca,
    y = deluca_freq,
    color = category,
    shape = category
  )
) +
  geom_jitter(size = 2.5, alpha = 0.75, width = 0.05, height = 0) +
  labs(
    x = "DeLuca Observations (total count)",
    y = "Proportion of DeLuca / Florida observations",
    color = "Category",
    shape = "Category"   # <-- this line merges the legends
  ) +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Unique to DeLuca" = "firebrick",
      "Locally Rare" = "mediumpurple3",
      "Rare Everywhere" = "darkolivegreen3",
      "Common Everywhere" = "lightskyblue2",
      "Underreported Everywhere" = "darkgoldenrod1"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Unique to DeLuca" = 17,
      "Locally Rare" = 15,
      "Rare Everywhere" = 16,
      "Common Everywhere" = 18,
      "Underreported Everywhere" = 19
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

freq_plot_prop_clean

## save as png
ggsave("Figures/Figure_5.png", plot = freq_plot_prop_clean, width = 10, height = 7, bg = "transparent")
