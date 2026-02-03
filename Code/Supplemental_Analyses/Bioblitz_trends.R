# Supplemental Analysis - Figure S4

# Determine popularity of bioblitzes by year in terms of it's presence in 
# the scientific literature and from iNaturalist projects

library(tidyverse)

#read in data
bioblitz_trends <- read_csv("Data/Supplemental_Analysis/bioblitz_trends.csv")

google <- bioblitz_trends %>%
  filter(Source == "Google Scholar")

inat <- bioblitz_trends %>%
  filter(Source == "iNaturalist Projects")

# Find scaling factor to align ranges
scale_factor <- max(google$n, na.rm=TRUE) / max(inat$n, na.rm=TRUE)

ggplot() +
  geom_line(data = google, aes(x = Year, y = n, color = "Google Scholar"), size = 1) +
  geom_line(data = inat, aes(x = Year, y = n * scale_factor, color = "iNaturalist projects"), size = 1) +
  scale_y_continuous(
    name = "Number of Articles on Google Scholar",
    sec.axis = sec_axis(~./scale_factor, name = "Number of iNaturalist Projects")
  ) +
  scale_color_manual(values = c("Google Scholar" = "firebrick", 
                                "iNaturalist projects" = "darkolivegreen3")) +
  scale_x_continuous(breaks = seq(2010, 2022, 2), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.y.left = element_text(color = "firebrick", face="bold"),
    axis.title.y.right = element_text(color = "darkolivegreen3", face="bold"),
    legend.position = "bottom"
  ) +
  labs(x = "Year", color = "Source")

ggsave("Figures/Supplemental/Figure_S4.jpg", width=5, height=4)




