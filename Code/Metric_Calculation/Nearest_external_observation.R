# Nearest observation for species with less than 100 observations

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# read in count data from the bioblitzes
count_data <- read_csv("Data/Summarized_Data/regional_species_counts.csv")

# now determine which species have less than 100 observations
rare_obs <- count_data %>%
  filter(deluca_bioblitz < 100)

# read in bioblitz preserve shapefile
deluca_sf <- st_read("Data/Shapefile/deluca.shp")

# read in all observations from Florida
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

# filter all data so it only includes relevant species
all_data_rel <- inat_combined_research %>% 
  filter(quality_grade=="research") %>%
  dplyr::select(latitude, longitude, taxon_species_name) %>%
  rename(species=taxon_species_name) %>%
  filter(species %in% rare_obs$Species)

# convert data into a shapefile
all_dat_sf <- st_as_sf(all_data_rel, coords = c("longitude", "latitude"), crs = 4326)

# Convert the projection to UTM so that we can get distance in meteres
all_dat_proj <- st_transform(all_dat_sf, 32617)
deluca_proj <- st_transform(deluca_sf, 32617)

# Remove observations that are inside of the preserve
inside_deluca <- st_within(all_dat_proj, deluca_proj, sparse = FALSE)[,1]
outside_deluca <- all_dat_proj[!inside_deluca, ]

# Calculate distances from all points to the polygon (returns a vector of distances)
outside_deluca$dist_to_deluca <- st_distance(outside_deluca, deluca_proj) %>% as.numeric()

# Finally, get the nearest observation
nearest_obs <- outside_deluca %>%
  as.data.frame() %>%
  group_by(species) %>%
  slice_min(order_by = dist_to_deluca, n = 1) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(species, dist_to_deluca)

# examine if there are any species that were not present in the iNaturalist data
(species_list <- setdiff(rare_obs$Species, nearest_obs$species))
# there are 4 not present in our data, related to the nearest observation being outside of Florida.

# manually extract data on these species from the iNaturalist API
get_inat_obs <- function(species_name, max_obs = 7000, delay = 1) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  per_page <- 200  # max allowed per request
  all_results <- list()
  page <- 1
  
  repeat {
    Sys.sleep(delay)  # pause to avoid hitting rate limits
    
    response <- GET(base_url, query = list(
      taxon_name = species_name,
      quality_grade = "research",
      per_page = per_page,
      page = page
    ))
    
    if (status_code(response) != 200) {
      warning(paste("Failed for", species_name, "on page", page))
      break
    }
    
    content_data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content_data, flatten = TRUE)
    
    results <- json_data$results
    
    if (length(results) == 0) break  # no more data
    
    all_results[[page]] <- results
    page <- page + 1
    
    # Stop if we've reached the max desired
    if ((page - 1) * per_page >= max_obs) break
  }
  
  # Combine all pages into one data frame
  if (length(all_results) == 0) return(NULL)
  
  obs_df <- bind_rows(all_results)
  obs_df$species_name <- species_name
  return(obs_df)
}

all_obs <- lapply(species_list, get_inat_obs)
inat_data <- bind_rows(all_obs)

# filter all data so it only includes relevant species
add_data_rel <- inat_data %>%
  filter(
    map_lgl(geojson.coordinates, ~ is.numeric(.x) && length(.x) == 2)
  ) %>%
  dplyr::select(geojson.coordinates, species = species_name) %>%
  mutate(
    lon = map_dbl(geojson.coordinates, 1),
    lat = map_dbl(geojson.coordinates, 2)
  )

# convert to a shapefile
add_data_sf <- st_as_sf(add_data_rel, coords = c("lon", "lat"), crs = 4326)

# Convert the projection to UTM so that we can get distance in meteres
add_dat_proj <- st_transform(add_data_sf, 32617)

# Remove observations that are inside of the preserve
inside_deluca <- st_within(add_dat_proj, deluca_proj, sparse = FALSE)[,1]
outside_deluca_add <- add_dat_proj[!inside_deluca, ]

# Calculate distances from all points to the polygon (returns a vector of distances)
outside_deluca_add$dist_to_deluca <- st_distance(outside_deluca_add, deluca_proj) %>% as.numeric()

# Finally, get the nearest observation
nearest_obs_add <- outside_deluca_add %>%
  as.data.frame() %>%
  group_by(species) %>%
  slice_min(order_by = dist_to_deluca, n = 1) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(species, dist_to_deluca)

# bind data frames together
nearest_obs_all <- rbind(nearest_obs, nearest_obs_add)

# double check that we have now captured all the species
(species_list <- setdiff(rare_obs$Species, nearest_obs_all$species))

# summary of data
nearest_obs_all <- read_csv("Data/Summarized_Data/distance_to_nearest_obs.csv")

summary(nearest_obs_all$dist_to_deluca)


