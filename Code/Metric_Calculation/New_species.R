# Get measure of new species at the site based on sampling effort

# packages
library(tidyverse)
library(patchwork)

# read in data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# clean bioblitz data
deluca_bioblitz <- deluca_bioblitz %>%
  filter(observed_on != "2023-02-25")  %>%
  mutate(year = year(observed_on)) # add year column

# make a histogram of RG species observed only
species_hist_RG <- deluca_bioblitz %>%
  dplyr::select(taxon_species_name, quality_grade) %>%
  dplyr::filter(quality_grade == "research") %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name) %>%
  summarize(N = n())

species_hist_plot_RG <- ggplot(species_hist_RG, aes(x = N)) +
  geom_histogram(color = "black", fill = "gray80") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(color = "black")
  ) +
  xlab("Number of observations") +
  ylab("Number of species")

# plot an accumulation type plot of new species
first_obs_species_all <- deluca_bioblitz %>%
  dplyr::select(taxon_genus_name, taxon_species_name, observed_on, quality_grade) %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name, observed_on) %>%
  arrange(desc(observed_on)) %>%
  distinct() %>%
  group_by(observed_on) %>%
  summarise(new_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  mutate(cumulative_species = cumsum(new_species))

accum_all_plot <- ggplot(first_obs_species_all, aes(x = observed_on)) +
  geom_col(aes(y = new_species), fill = "black", alpha = 0.6, width = 4) +
  geom_line(aes(y = cumulative_species), color = "darkgoldenrod1", size = 1.2) +
  geom_point(aes(y = cumulative_species), size = 2, color = "darkgoldenrod1") +
  xlab("Sampling Date") +
  ylab("Species count (RG + Needs ID)") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(color = "black")
  )

# plot an accumulation type plot of new species
first_obs_species_RG <- deluca_bioblitz %>%
  filter(
    quality_grade == "research",
    !is.na(taxon_species_name)
  ) %>%
  group_by(taxon_species_name) %>%
  summarise(
    first_date = min(observed_on),
    .groups = "drop"
  )

# count number of species 
accum_data_RG <- first_obs_species_RG %>%
  group_by(first_date) %>%
  summarise(new_species = n(), .groups = "drop") %>%
  arrange(first_date) %>%
  mutate(cumulative_species = cumsum(new_species))

# now plot
accum_RG_plot <- ggplot(accum_data_RG, aes(x = first_date)) +
  geom_col(aes(y = new_species), fill = "black", width = 6) +
  geom_line(aes(y = cumulative_species), color = "darkgoldenrod1", size = 1.2) +
  geom_point(aes(y = cumulative_species), color = "darkgoldenrod1", size = 2) +
  labs(
    x = "Sampling Date",
    y = "Number of species"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(color = "black")
  )

# Figure 3
final_figure_3 <- species_hist_plot_RG + accum_RG_plot +
  plot_annotation(tag_levels = "A")
final_figure_3

ggsave("Figures/Figure_3.png", plot = final_figure_3, bg = "transparent",dpi = 300)
