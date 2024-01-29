
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