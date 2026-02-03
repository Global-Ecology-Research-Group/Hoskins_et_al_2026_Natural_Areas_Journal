# Generate 100 random polygons in the state of Florida then use the 
# iNaturalist API to get number of iNaturalist observations, species, and
# observers in each polygon

# This data is used to get Regional-Level total observation, number of species
# and number of observations to compare to the site

library(tidyverse)
library(tigris)
library(sf)
library(dplyr)
library(units)

# Get random polygons in Florida ------------------------------------------

# read in bioblitz polygon
deluca_sf <- st_read("Data/Shapefile/deluca.shp")

# get a shapefile of Florida
florida_sf <- states(cb = TRUE) %>%
  filter(STUSPS == "FL") %>%
  st_transform(st_crs(deluca_sf))  # Match CRS with deluca

# Convert to UTM, which is better for distance calculations
proj_crs <- 32617
deluca_proj <- st_transform(deluca_sf, proj_crs)
florida_proj <- st_transform(florida_sf, proj_crs)
florida_union <- st_union(florida_proj)

# get the area of the bioblitz to ensure that random squares are the same size
target_area <- as.numeric(st_area(deluca_proj))
side_length <- sqrt(target_area)
half_side <- side_length / 2

# Function to create square polygon centered at a point
create_square <- function(center_coords, half_side) {
  coords <- matrix(c(
    center_coords[1] - half_side, center_coords[2] - half_side,  # bottom-left
    center_coords[1] - half_side, center_coords[2] + half_side,  # top-left
    center_coords[1] + half_side, center_coords[2] + half_side,  # top-right
    center_coords[1] + half_side, center_coords[2] - half_side,  # bottom-right
    center_coords[1] - half_side, center_coords[2] - half_side   # close polygon
  ), ncol = 2, byrow = TRUE)
  st_polygon(list(coords))
}

# Parameters
n <- 100
set.seed(2025)
random_squares <- list()
attempts <- 0
max_attempts <- 10000  # to avoid infinite loops

while (length(random_squares) < n & attempts < max_attempts) {
  attempts <- attempts + 1
  # sample one random point inside Florida polygon
  pt <- st_sample(florida_union, 1, type = "random")
  pt_coords <- st_coordinates(pt)
  
  # create square polygon (sfg)
  square <- create_square(pt_coords, half_side)
  
  # check if square is fully inside Florida
  if (!st_within(st_sfc(square, crs = proj_crs), florida_union, sparse = FALSE)[1]) {
    next  # skip if outside Florida
  }
  
  # check overlap with existing squares
  if (length(random_squares) > 0) {
    existing_sfc <- st_sfc(random_squares, crs = proj_crs)
    if (any(st_intersects(st_sfc(square, crs = proj_crs), existing_sfc, sparse = FALSE))) {
      next  # skip if overlaps
    }
  }
  
  # add square if passed checks
  random_squares[[length(random_squares) + 1]] <- square
  
  if (length(random_squares) %% 10 == 0) {
    message(paste("Accepted", length(random_squares), "squares after", attempts, "attempts"))
  }
}

# Convert to sf
random_squares_sf <- st_sf(
  id = 1:length(random_squares),
  geometry = st_sfc(random_squares, crs = proj_crs)
) %>%
  mutate(size = as.numeric(st_area(geometry)))

# Plot the data and save a supplemental figure
png("Figures/Supplemental/Figure_S1.png", width = 8, height = 6, units = "in", res = 300)
plot(st_geometry(florida_proj), col = "lightgrey", border = NA)
plot(st_geometry(random_squares_sf), col = rgb(1, 0, 0, 0.8), border = NA, add = TRUE)
plot(st_geometry(deluca_proj), col = "black", border = NA, add = TRUE)
legend(x = par("usr")[1] + 0.2 * diff(par("usr")[1:2]),  # X position: a bit to the right of the left edge
       y = par("usr")[3] + 0.3 * diff(par("usr")[3:4]),  # Y position: slightly above bottom
       legend = c("Florida", "Random Polygons", "DeLuca Preserve"),
       fill = c("lightgrey", rgb(1, 0, 0, 0.8), "black"),
       border = NA,
       bty = "n",
       xpd = NA)
dev.off()

# Check to make sure the size aligns with the target area
square_polygons_sf <- random_squares_sf %>%
  mutate(size = as.numeric(st_area(geometry)))

summary(square_polygons_sf$size)  # should be close to target_area
target_area

# Get bounding boxes of each polygon
bbox_list <- map(st_geometry(random_squares_sf), function(x) {
  bbox_vec <- as.numeric(st_bbox(x))
  names(bbox_vec) <- c("xmin", "ymin", "xmax", "ymax")
  bbox_vec
})

bbox_df <- map_dfr(bbox_list, ~ as.data.frame(t(.)))

random_squares_sf_with_bbox <- random_squares_sf %>%
  bind_cols(bbox_df) 

# Convert bbox from UTM to WGS84 for iNaturalist API call
random_squares_wgs <- st_transform(random_squares_sf_with_bbox, 4326)

# Extract bbox correctly from geometry
random_squares_wgs <- random_squares_wgs %>%
  rowwise() %>%
  mutate(
    bbox = list(st_bbox(geometry)),
    sw_lng = bbox$xmin,
    sw_lat = bbox$ymin,
    ne_lng = bbox$xmax,
    ne_lat = bbox$ymax
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  select(sw_lng, sw_lat, ne_lng, ne_lat) 

# Check bbox for the first polygon:
print(random_squares_wgs %>% slice(1) %>% select(sw_lng, sw_lat, ne_lng, ne_lat))

# Get iNat data -----------------------------------------------------------

# Function to get observation counts (research grade only)
get_obs_count <- function(sw_lng, sw_lat, ne_lng, ne_lat) {
  url <- "https://api.inaturalist.org/v1/observations"
  
  res <- GET(url, query = list(
    swlng = sw_lng,
    swlat = sw_lat,
    nelng = ne_lng,
    nelat = ne_lat,
    quality_grade = "research",
    per_page = 1,
    page = 1
  ))
  
  if (res$status_code != 200) return(NA)
  
  content_text <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(content_text)
  
  return(json$total_results)
}

# Function to get number of species in bbox (research grade only)
get_species_count <- function(sw_lng, sw_lat, ne_lng, ne_lat) {
  url <- "https://api.inaturalist.org/v1/observations/species_counts"
  
  res <- GET(url, query = list(
    swlng = sw_lng,
    swlat = sw_lat,
    nelng = ne_lng,
    nelat = ne_lat,
    quality_grade = "research",
    per_page = 1  # we only need metadata, not all results
  ))
  
  if (res$status_code != 200) return(NA)
  
  content_text <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(content_text)
  
  return(json$total_results)
}

# Function to get number of observers in bbox (research grade only)
get_observer_count <- function(sw_lng, sw_lat, ne_lng, ne_lat) {
  url <- "https://api.inaturalist.org/v1/observations/observers"
  
  res <- GET(url, query = list(
    swlng = sw_lng,
    swlat = sw_lat,
    nelng = ne_lng,
    nelat = ne_lat,
    quality_grade = "research",
    per_page = 1,  # we only need metadata, not full results
    page = 1
  ))
  
  if (res$status_code != 200) return(NA)
  
  content_text <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(content_text)
  
  return(json$total_results)
}

# Use these functions to calculate summaries across all random squares

# Create polygon_id and bbox string
random_squares_wgs <- random_squares_wgs %>%
  mutate(
    polygon_id = row_number()
  )

# Wrapper function for safely getting all metrics for a single row
get_metrics <- function(sw_lng, sw_lat, ne_lng, ne_lat) {
  list(
    number_of_observations = get_obs_count(sw_lng, sw_lat, ne_lng, ne_lat),
    number_of_observers = get_observer_count(sw_lng, sw_lat, ne_lng, ne_lat),
    number_of_species = get_species_count(sw_lng, sw_lat, ne_lng, ne_lat)
  )
}

# Apply across all rows
results <- pmap_dfr(
  list(
    sw_lng = random_squares_wgs$sw_lng,
    sw_lat = random_squares_wgs$sw_lat,
    ne_lng = random_squares_wgs$ne_lng,
    ne_lat = random_squares_wgs$ne_lat
  ),
  ~{
    # Wait 2 seconds between calls to abide by API call limits
    Sys.sleep(2)  
    
    metrics <- get_metrics(..1, ..2, ..3, ..4)
    
    tibble(
      number_of_observations = metrics$number_of_observations,
      number_of_observers = metrics$number_of_observers,
      number_of_species = metrics$number_of_species
    )
  }
)

# Bind with metadata
final_df <- bind_cols(
  polygon_id = random_squares_wgs$polygon_id,
  sw_lng = random_squares_wgs$sw_lng,
  sw_lat = random_squares_wgs$sw_lat,
  ne_lng = random_squares_wgs$ne_lng,
  ne_lat = random_squares_wgs$ne_lat,
  results
)

print(final_df)

write_csv(final_df_all, "Data/Summarized_Data/random_polygon_effort.csv")
