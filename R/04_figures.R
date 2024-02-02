# graphs for manuscript
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))

# spat over time ----------------------------------------------------------
a <- spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_classic(base_family = "serif") +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell")

ggsave(a, filename = here('output', 'spat-over-time.png'), dpi = 300, units = "in", width = 5, height = 3)

# spat by region with stats -----------------------------------------------


# spat by year with stats -------------------------------------------------


# settlement period -------------------------------------------------------


# monthly water quality parameters over time ------------------------------


