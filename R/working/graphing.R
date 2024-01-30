
# plot with all sites, faceted by year for dual x axis, and error zero --------

dat %>% 
  ggplot(aes(x = soak_month, y = spat_count_region, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  facet_grid(~ year(soak_month), space="free_x", scales="free_x", switch="x") +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_spat() +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  labs(x = "", y = "Mean Spat Per Shell")

dat_soak %>% 
  ggplot(aes(x = soak_month, y = spat_std_count_region, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = spat_std_count_region - spat_std_count_se, ymax = spat_std_count_region + spat_std_count_se)) +
  facet_grid(~ year(soak_month), space="free_x", scales="free_x", switch="x") +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_spat() +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  labs(x = "", y = "Mean Spat Per Shell Per Day")

# plot with all sites, only time as years in x axis, error bars -----------

dat %>% 
  ggplot(aes(x = soak_month, y = spat_count_region, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_spat() +
  theme(legend.position = "top") +
  labs(x = "", y = "Mean Spat Per Shell")

dat_soak %>% 
  ggplot(aes(x = soak_month, y = spat_std_count_region, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = spat_std_count_region - spat_std_count_se, ymax = spat_std_count_region + spat_std_count_se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_spat() +
  theme(legend.position = "top") +
  labs(x = "", y = "Mean Spat Per Shell Per Day")


# individual regions, faceted by year, error bars, and color filte --------

dat %>% 
  mutate(year = as.character(year(soak_month)),
         month = month(soak_month, label = T)) %>%
  filter(region_friendly == "TR") %>% 
  ggplot(aes(x = month, y = spat_count_region, group = year, color = spat_count_region > 0)) +
  geom_point() +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  facet_wrap(~year, ncol = 1) +
  theme_classic()


# plot of annual sums by region with SE bars ------------------------------

dat %>% 
  mutate(year = lubridate::year(soak_month)) %>% 
  group_by(year, region_friendly) %>% 
  summarize(mean = mean(spat_count_region, na.rm = T),
            se = (sd(spat_count_region, na.rm = T)/sqrt(length(spat_count_region)))) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_bar(aes(fill = region_friendly, color = region_friendly),
           stat = "identity", width = 0.5) +
  geom_errorbar(aes(x = year, ymin = mean - se, ymax = mean + se), width = 0.4) +
  facet_grid(rows = vars(region_friendly), scales = "free_y") +
  scale_fill_manual(values = sitecolours) +
  scale_color_manual(values = sitecolours) +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean annual spat settlement")

dat %>% 
  mutate(year = lubridate::year(soak_month)) %>% 
  group_by(year, region_friendly) %>% 
  summarize(mean = mean(spat_count_region, na.rm = T),
            se = (sd(spat_count_region, na.rm = T)/sqrt(length(spat_count_region)))) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_bar(fill = "white", stat = "identity", color = "black", width = 0.5) +
  geom_errorbar(aes(x = year, ymin = mean - se, ymax = mean + se), width = 0.4) +
  facet_grid(rows = vars(region_friendly), scales = "free_y") +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean annual spat settlement")
