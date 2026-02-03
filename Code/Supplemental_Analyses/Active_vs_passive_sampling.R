# Supplemental Analysis - Figure S5

# Determine how number of observations and number of species reported at the DeLuca
# bioblitz (active sampling) compare to 39 state parks where passive sampling occurred.

# Data for 39 state parks comes from: Lowe, S. K., Mason, B. M., Morales, N. A., & 
# Callaghan, C. T. (2026). Participatory citizen science data complements agency‐collected 
# data for species inventories. Ecological Solutions and Evidence, 7(1), e70173.

library(tidyverse)
library(sf)
library(units)
library(sjPlot)
library(DHARMa)
library(MASS)
library(patchwork)
sf_use_s2(FALSE)

# read in iNat data for parks
inat_parks <- readRDS("Data/Supplemental_Analysis/iNat_District2.RDS") %>% st_as_sf()

# read in shapefile of wildlife management areas
wma_fl <- st_read("Data/Supplemental_Analysis/Florida_State_Parks_Boundaries.shp")

inat_parks_sf <- st_transform(inat_parks, st_crs(wma_fl))

# get park name added to iNat data
inat_parks_wma <- st_join(
  inat_parks_sf,
  wma_fl[, c("SITE_NAME", "ACREAGE")],   # keep only what you need
  join = st_within
)

# filter out parks that were not in Lowe et al. paper
parks_rel <- read_csv("Data/Supplemental_Analysis/species_by_park_and_source.csv")

# get relevant data
inat_wma <- inat_parks_wma %>%
  as.data.frame() %>%
  mutate(date = parse_date_time(eventDate, orders = c("ymd HMS", "ymd HM", "ymd", "ym", "y"), tz = "UTC")) %>%
  dplyr::select(gbifID, date, species, SITE_NAME.x, ACREAGE.x) %>%
  rename(id=gbifID, park=SITE_NAME.x, park_size=ACREAGE.x) %>%
  mutate(park_size=park_size*0.4046856421,
         park = ifelse(park=="O'Leno State Park", "O'Leno State Park and River Rise Preserve State Park", park)) %>%
  filter(complete.cases(park),
         park %in% unique(parks_rel$parks))

# now get DeLuca data
deluca <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# get deluca shapefile so we can get the park size
deluca_sf <- st_read("Data/Shapefile/deluca.shp")

# get square miles to match the wma
area_sq_m <- st_area(deluca_sf)
area_sq_ha <- set_units(area_sq_m, ha)
area_sq_ha

deluca_clean <- deluca %>%
  mutate(date=ymd(observed_on)) %>%
  dplyr::select(id, date, taxon_species_name) %>%
  rename(species=taxon_species_name) %>%
  mutate(park="DeLuca",
         park_size=area_sq_ha)

wma_deluca <- rbind(inat_wma, deluca_clean)

# summarize data
wma_deluca_sum <- inat_wma %>%
  group_by(park) %>%
  summarise(observations = n(), 
            species = n_distinct(species),
            park_size=first(park_size))
wma_deluca_sum


plot(observations ~ park_size, data=wma_deluca_sum)

# now model the data to compare other parks to DeLuca
mod_obs <- glm.nb(
  observations ~ log(park_size),
  data = wma_deluca_sum
)
summary(mod_obs)

sim_res <- simulateResiduals(mod_obs, n = 1000)
plot(sim_res)

plot_model(mod_obs, type = "pred", terms = "park_size")

newdata <- data.frame(park_size = seq(min(wma_deluca_sum$park_size),
                                      max(wma_deluca_sum$park_size),
                                      length.out = 100))

# Predict species richness with standard errors
preds <- predict(mod_obs, newdata = newdata, type = "link", se.fit = TRUE)

newdata <- newdata %>%
  mutate(
    fit = exp(preds$fit),                 # NB link is log, back-transform
    lower = exp(preds$fit - 1.96 * preds$se.fit),
    upper = exp(preds$fit + 1.96 * preds$se.fit)
  )

obs <- ggplot(wma_deluca_sum, aes(x = park_size, y = observations)) +
  geom_ribbon(data = newdata, aes(x = park_size, ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = newdata, aes(x = park_size, y = fit),
            color = "firebrick", linewidth = 1.2, inherit.aes = FALSE) +   # fitted line
  # Highlight specific parks
  geom_point(aes(x = as.numeric(deluca_clean$park_size[1])), y = nrow(deluca_clean),
             color = "black", size = 4) +
  geom_text(aes(x = as.numeric(deluca_clean$park_size[1]),
                y = nrow(deluca_clean),
                label = "DeLuca Bioblitzes"),
            vjust = 2, hjust = 0.5, size = 4, fontface = "bold") +
  scale_x_continuous(name = "Park size (ha)") +
  scale_y_continuous(name = "Number of observations") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) 
obs 

# do the same with species
# now model the data to compare other parks to DeLuca
mod_obs_sp <- glm.nb(
  species ~ log(park_size),
  data = wma_deluca_sum
)
summary(mod_obs_sp)

plot_model(mod_obs_sp, type = "pred", terms = "park_size")

sim_res <- simulateResiduals(mod_obs_sp, n = 1000)
plot(sim_res)

# Create a sequence of park sizes for predictions
newdata <- data.frame(park_size = seq(min(wma_deluca_sum$park_size),
                                      max(wma_deluca_sum$park_size),
                                      length.out = 100))

# Predict species richness with standard errors
preds <- predict(mod_obs_sp, newdata = newdata, type = "link", se.fit = TRUE)

newdata <- newdata %>%
  mutate(
    fit = exp(preds$fit),                 # NB link is log, back-transform
    lower = exp(preds$fit - 1.96 * preds$se.fit),
    upper = exp(preds$fit + 1.96 * preds$se.fit)
  )

sp <- ggplot(wma_deluca_sum, aes(x = park_size, y = species)) +
  geom_ribbon(data = newdata, aes(x = park_size, ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = newdata, aes(x = park_size, y = fit),
            color = "firebrick", linewidth = 1.2, inherit.aes = FALSE) +   # fitted line
  # Highlight specific parks
  geom_point(aes(x = as.numeric(deluca_clean$park_size[1])), y = length(unique(deluca_clean$species)),
             color = "black", size = 4) +
  geom_text(aes(x = as.numeric(deluca_clean$park_size[1]),
                y = length(unique(deluca_clean$species)),
                label = "DeLuca Bioblitzes"),
            vjust = 2, hjust = 0.5, size = 4, fontface = "bold") +
  scale_x_continuous(name = "Park size (ha)") +
  scale_y_continuous(name = "Number of species") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) 
sp

combined <- obs + sp + 
  plot_annotation(tag_levels = 'A')  # labels A, B automatically

# Print the combined plot
combined

ggsave("Figures/Supplemental/Figure_S5.jpeg", height=6, width=10, units="in")
