# For all species observed during bioblitzes, get the count of observations for the 
# county, state, and in all of iNaturalist using the API

library(tidyverse)
library(httr)
library(jsonlite)

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# Filter to only include research grade data
deluca_bioblitz <- deluca_bioblitz %>%
  filter(quality_grade=="research")

# get a list of species
species <- unique(deluca_bioblitz$taxon_species_name)

# get the place ID for the three regions of interest
place_ids <- data.frame(region=c("Osceola County", "Florida", "All"),
                       place_id=c(938,21,0))

# use the API to get a count of observations for each species by region
get_obs_count <- function(scientific_name, place_id = NA) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Construct query
  query <- list(
    taxon_name = scientific_name,
    quality_grade = "research",
    per_page = 1,
    page = 1
  )
  
  # Only include place_id if not 0 or NA
  if (!is.na(place_id) && place_id != 0) {
    query$place_id <- place_id
  }
  
  res <- httr::GET(url = base_url, query = query)
  
  if (res$status_code == 200) {
    Sys.sleep(5)  # Delay after a successful call
    json <- httr::content(res, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(json, flatten = TRUE)
    return(parsed$total_results)
  } else {
    warning(paste("Failed for", scientific_name, "in place", place_id))
    return(NA)
  }
}

results <- expand.grid(Species = species, Region = place_ids$region, stringsAsFactors = FALSE)

results <- results %>%
  left_join(place_ids, by = c("Region" = "region")) %>%
  rowwise() %>%
  mutate(Observations = get_obs_count(Species, place_id)) %>%
  ungroup() %>%
  select(Species, Region, Observations) %>%
  pivot_wider(names_from = Region, values_from = Observations)

# add the total number of observations from the bioblitz
deluca_bioblitz_count <- deluca_bioblitz %>%
  group_by(taxon_species_name) %>%
  summarise(deluca_bioblitz=n())

results_w_deluca <- left_join(results, deluca_bioblitz_count, by=c("Species"="taxon_species_name"))

write_csv(results_w_deluca, "Data/Summarized_Data/regional_species_counts.csv")
