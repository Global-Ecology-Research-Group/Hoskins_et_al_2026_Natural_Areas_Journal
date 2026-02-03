This repository contains all the code and files used in the analyses reported in "Bioblitzes provides valuable biodiversity data" article, which is published in Natural Areas Journal

# Code Folder

This folder contains all R scripts which can be used to repeat the results presented in the article and supplemental material. The R scripts are structured based on the core metrics calculated for the paper (Table 1).

**Table 1.** Metrics used to assess bioblitz biodiversity utility. Metric scale is defined as site-level (SL) which characterizes biodiversity patterns within the site defined for the bioblitz and regional-level (RL) which contextualizes bioblitz observations relative to broader regional biodiversity data. We additionally provide a brief definition of each metric. The script column details which script is used to calculate each metric. All scripts are located in the Code/Metric_Calculation file path in the repository.

| Metric | Definition | Script |
|--------------|---------------------------|--------------------------------|
| Total observations (SL) | Total observations taken at the site. | Total_observations_observers_species.R |
| Total observations (RL) | The relative number of observations at the site compared to the surrounding region. | Total_observations_observers_species.R |
| Number of observers (SL) | Unique observers who submitted observations at the site. | Total_observations_observers_species.R |
| Number of observers (RL) | The relative number of observers at the site compared to the surrounding region. | Total_observations_observers_species.R |
| Number of species (SL) | Number of unique species observed at the site. | Total_observations_observers_species.R |
| Number of species (RL) | The relative number of unique species at the site compared to the surrounding region. | Total_observations_observers_species.R |
| New species (SL) | Measure of new species at the site based on sampling effort. | New_species.R |
| Relative frequency (RL) | Frequency of species at the site relative to the region. | Relative_frequency_and_rarity.R |
| Relative rarity (RL) | Measure of species rarity based on number of observations at the site and region. | Relative_frequency_and_rarity.R |
| Relative diversity of species (RL) | Diversity of species at the site compared to the region. | Relative_diversity_of_species.R |
| Nearest external observation (RL) | Distance from the site to the nearest external observation for species with fewer than 100 site-level observations. | Nearest_external_observation.R |

: Below is a description of each script:

**Data_Preparation –** This folder contains scripts used to prepare data for metric calculation:

-   **Effort_in_random_polygons.R –** Code to create 100 random polygons in Florida and get summary of total observations, number of observers, and number of species in each polygon. The output of this script is used to get regional-level metrics for total observations, number of observers, and number of species. This code additionally outputs figure S1.

-   **Generate_species_counts.R –** Code to get a count of observations in the county, state, and all of iNaturalist for all species observed during the bioblitz. The output of this script is used in the code to get regional-level metrics for total observations, number of observers, and number of species, relative frequency, relative rarity, and nearest external observation.

**Metric_Calculation –** This folder contains the code used to calculate the metrics described in Table 1:

-   **Total_observations_observers_species.R –** Code to calculate site-level and regional level total observations, number of observers, and number of species metrics. This code outputs figures 2 and 4.

-   **New_species.R –** Code to calculate new species at the site based on sampling effort via analysis of species accumulation curve. This code outputs figure 3.

-   **Relative_frequency_and_rarity.R –** Code to calculate the relative frequency and rarity of species at the site compared to the region. This code outputs figure S3 and figure 5.

-   **Relative_diversity_of_species.R –** Code to calculate the relative diversity of species at the site compared to the region.

-   **Nearest_external_observation.R –** Code to calculate the distance from the site to the nearest external observation for species with fewer than 100 site-level observations.

**Supplemental_Analyses –** This folder contains code used for supplemental analysis:

-   **Endemic_introduced_species.R –** Code to determine introduced and endemic status of all species observed at the site, and then plot the number of introduced and endemic species over time. This code outputs figure S2.

-   **Active_vs_passive_sampling.R –** Code to determine how number of observations and number of species reported at the site (active sampling) compare to 39 state parks where passive sampling occurred.Data for 39 state parks comes from: Lowe, S. K., Mason, B. M., Morales, N. A., & Callaghan, C. T. (2026). Participatory citizen science data complements agency‐collected data for species inventories. Ecological Solutions and Evidence, 7(1), e70173. This code outputs figure S5.

-   **Bioblitz_trends.R –** Code to determine the popularity of bioblitzes by year in terms of it's presence in the scientific literature and from iNaturalist projects. This code outputs figure S4.

# Data

This folder contains all data used in the analyses. The following is a description of data present in this repository:

**DeLuca_iNaturalist_Data –** This folder contains iNaturalist data from the DeLuca Preserve:

-   **deluca_bioblitz_obs.csv –** All iNaturalist data taken during the four DeLuca bioblitz events. This data was downloaded using the data export tool on iNaturalist on July 1, 2025. We chose to use the iNaturalist data export tool instead of GBIF because it allows us to filter by iNaturalist project (project ID: annual-uf-ifas-deluca-bioblitz-data), which is not possible in GBIF.

-   **deluca_obs.csv –** All iNaturalist data taken within the DeLuca Preserve boundary. This data was downloaded using the data export tool on iNaturalist on July 1, 2025. We chose to use the iNaturalist data export tool instead of GBIF because it allows us to filter by iNaturalist place (place ID: 212812, UF/IFAS DeLuca Preserve), which is not possible in GBIF.

-   **species_status_florida.csv –** For all species observed during the bioblitz, we obtained the species status using the iNaturalist API (see `Supplemental_Analyses/Endemic_introduced_species.R`). This file contains each species observed during the bioblitz and their associated status.

**Florida_Data –** This folder contains all the iNaturalist data in Florida. To reduce individual file sizes for ease of use on GitHub, data is divided into 65 CSV files. Data was downloaded on June 27, 2025 using the iNaturalist data export tool so species names align with deluca_bioblitz_obs.csv and deluca_obs.csv data frames.

**Shapefile –** This folder contains a shapefile for the DeLuca Preserve. The boundary of the DeLuca Preserve changed slightly in 2025 compared to the years 2022-2024. This shapefile represents the most recent boundary, which was used for the 2025 bioblitz.

**Summarized_Data –** This folder contains data summaries to answer the research questions posed in the paper. The data is as follows:

-   **distance_to_nearest_obs.csv –** For all species that had less than 100 research grade iNaturalist observations during the DeLuca bioblitzes, we calculated the distance to the nearest observation of that species excluding observations taken within the DeLuca Preserve shapefile. The distance is presented as meters. This dataset is an output of `Metric_Calculation/Nearest_external_observation.R`.

-   **random_polygon_effort.csv –** For this data set, we randomly generated 100 rectangles in the state of Florida which matched the size as the DeLuca Preserve shapefile. Next, we used the iNaturalist API to obtain the total number of observations, observers, and species within that rectangle. This data will be used to compare cumulative effort. This dataset is an output of `Data_Preparation/Effort_in_random_polygons.R`.

-   **regional_species_count.csv –** For all species recorded during DeLuca bioblitzes, we obtained the observation count of that species during the bioblitzes, in Osceola county, Florida, and the world. This dataset is an output of `Data_Preparation/Generate_species_counts.R`**.**

**Supplemental_Analysis –** Additional data files used for supplemental analyses:

-   **bioblitz_trends.csv –** Number of Google Scholar results when the term "bioblitz" was searched by year, and number of iNaturalist project results when the term "bioblitz" was searched by year. This dataset was used in the `Supplemental_Analyses``/Bioblitz_trends.R` script.

-   **Florida_State_Parks_Boundaries.shp –** Boundaries of select Florida State Parks as described in Lowe, S. K., Mason, B. M., Morales, N. A., & Callaghan, C. T. (2026). Participatory citizen science data complements agency‐collected data for species inventories. Ecological Solutions and Evidence, 7(1), e70173. This dataset was used in the `Supplemental_Analyses/Active_vs_passive_sampling.R` script.

-   **iNat_District2 –** iNaturalist observations used in Lowe, S. K., Mason, B. M., Morales, N. A., & Callaghan, C. T. (2026). Participatory citizen science data complements agency‐collected data for species inventories. Ecological Solutions and Evidence, 7(1), e70173. This data was used to get a summary of total observations and number of species by Florida state park. This dataset was used in the `Supplemental_Analyses/Active_vs_passive_sampling.R` script.

-   **species_by_park_and_source.csv –** List of parks that were used in Lowe, S. K., Mason, B. M., Morales, N. A., & Callaghan, C. T. (2026). Participatory citizen science data complements agency‐collected data for species inventories. Ecological Solutions and Evidence, 7(1), e70173. This dataset was used in the `Supplemental_Analyses/Active_vs_passive_sampling.R` script.

# Figures

This folder contains all the figures presented in the paper. A sub-folder named "Supplemental" contains all the supplemental figures.
