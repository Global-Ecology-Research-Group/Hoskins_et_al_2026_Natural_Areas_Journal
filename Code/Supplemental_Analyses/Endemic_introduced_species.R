# Supplementary analysis to compare cumulative number of endemic and introduced
# species recorded during the bioblitz

library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)


# Get species status ------------------------------------------------------

# Get the status of each species found in Florida based on 
# establishment means from iNaturalist using the API

# Base URL
base_url <- "https://api.inaturalist.org/v1/observations/species_counts"

# Parameters
params <- list(
  place_id = 21,
  include_ancestors = "false",
  per_page = 200, 
  page = 1
)

# loop to get observations
all_data <- list()
page <- 1
repeat {
  # update page number
  params$page <- page
  
  # request data
  res <- GET(base_url, query = params)
  
  # check for error
  stop_for_status(res)
  
  # parse JSON
  dat <- content(res, as = "parsed", simplifyVector = TRUE)
  
  # store results
  all_data[[page]] <- dat$results
  
  # stop if no more results
  if(length(dat$results) == 0) break
  
  page <- page + 1
  Sys.sleep(0.5)  # polite pause to avoid rate-limiting
}

# combine into a data frame
species_counts <- bind_rows(all_data)

# look at first rows
head(species_counts)

# simplify data 
simple_data <- data.frame(rank=species_counts$taxon$rank, 
                          name=species_counts$taxon$name,
                          establishment_means=species_counts$taxon$establishment_means$establishment_means)

write_csv(simple_data, "species_status_florida.csv")

# Summarize bioblitz data by status----------------------

# Read in the bioblitz data and species native/non-native status
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# Join the two datasets by scientific name
deluca_joined <- deluca_bioblitz %>%
  left_join(species_status_florida, by = c("scientific_name" = "name"))

# remove the odd ball date
deluca_joined <- deluca_joined %>%
  filter(observed_on != "2023-02-25")

## Filter to only include research grade data
deluca_joined_research <- deluca_joined %>%
  filter(quality_grade=="research")

# Make sure the date column is in Date format
deluca_joined_research <- deluca_joined_research %>%
  mutate(observed_on = as.Date(observed_on))

# Filter to only endemic and introduced species
endemic_introduced <- deluca_joined_research %>%
  filter(establishment_means %in% c("endemic", "introduced")) %>%
  distinct(scientific_name, establishment_means, observed_on)

# Get the first year each species was observed
species_first_seen <- endemic_introduced %>%
  mutate(year = year(observed_on)) %>%
  group_by(establishment_means, scientific_name) %>%
  summarise(first_year = min(year), .groups = "drop")

# Count cumulative species over time
cumulative_counts <- species_first_seen %>%
  group_by(establishment_means, first_year) %>%
  summarise(new_species = n(), .groups = "drop") %>%
  arrange(establishment_means, first_year) %>%
  group_by(establishment_means) %>%
  mutate(cumulative_species = cumsum(new_species)) %>%
  ungroup()

# Plot cumulative species over time
cumul_plot_endemic_invasive <- ggplot(
  cumulative_counts, aes(x = first_year, y = cumulative_species, color = establishment_means)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = c("endemic" = "firebrick", "introduced" = "darkolivegreen3")) +
  labs(
    x = NULL,
    y = "Cumulative Number of Unique Species",
    color = "Establishment Type",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# View the plot
cumul_plot_endemic_invasive

ggsave("Figures/Supplemental/Figure_S2.png", plot = cumul_plot_endemic_invasive, bg = "transparent")
