library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))


# plot with all sites, faceted by year for dual x axis, and error zero --------

spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), alpha = 0.7) +
  facet_grid(~ year(soak_month), space="free_x", scales="free_x", switch="x") +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_classic(base_family = "serif") +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.4, size = 9),
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell")

# plot with all sites, only time as years in x axis, error bars -----------

spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_classic() +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell")

# individual regions, faceted by year, error bars, and color filte --------

spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  mutate(year = as.character(year(soak_month)),
         month = month(soak_month, label = T)) %>%
  filter(region_friendly == "TR") %>% 
  ggplot(aes(x = month, y = mean, group = year, color = mean > 0)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  theme_classic() +
  scale_color_manual(values = c('gray', 'black')) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Spat Per Month",
       title = "TR: Tolomoto River")


# plot of annual sums by region with SE bars ------------------------------

spat %>% 
  group_by(year, region_friendly) %>% 
  summarize(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_bar(aes(fill = region_friendly, color = region_friendly),
           stat = "identity", width = 0.5) +
  geom_errorbar(aes(x = year, ymin = mean - se, ymax = mean + se), width = 0.4) +
  geom_point() +
  facet_grid(rows = vars(region_friendly), scales = "free_y") +
  scale_fill_manual(values = sitecolours) +
  scale_color_manual(values = sitecolours) +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean annual spat settlement")


# monthly avg spat settlement ---------------------------------------------

spat %>% 
  mutate(month = month(soak_month, label = T)) %>% 
  group_by(region_friendly, month)  %>% 
  summarize(mean = mean(spat_count, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_line() +
  # geom_point() +
  scale_color_manual(name = "Region", values = sitecolours) +
  theme_bw() +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean spat per shell") +
  geom_segment(aes(x = "Apr", y = 200, xend = "Oct", yend = 200), 
               color = "black", size = 2, linetype = "dashed") +
  annotate("text",
           label = "Settlement Period",
           x = "Jul",
           y = 195)
  
