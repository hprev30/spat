
label_range <- dat %>%
    mutate(year = year(retrieval_date),
           month = month(retrieval_date, label = T)) %>%
    group_by(year) %>%
    summarize(xmin = min(retrieval_date),
              xmax = max(retrieval_date),
              ymin = -0.5,
              ymax = ymin + 0.15)
  
dat %>% 
  filter(spat_count_region > 0) %>% 
  ggplot(aes(x = retrieval_date, y = spat_count_region)) +
  geom_point(aes(group = region_friendly, color = region_friendly), size = 2) +
  # geom_line() +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  # facet_wrap(~region_friendly, ncol = 1, scales = "free_y") +
  facet_grid(~ year(retrieval_date), space="free_x", scales="free_x", switch="x") +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  theme_spat() +
  # theme(legend.position = "bottom") +
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  labs(x = "", y = "Mean Spat Per Shell")

dat %>% 
  ggplot(aes(x = retrieval_date, y = spat_count_region, group = region_friendly, color = region_friendly)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_spat() +
  theme(legend.position = "top") +
  labs(x = "", y = "Mean Spat Per Shell")

dat %>% 
  mutate(year = as.character(year(retrieval_date)),
         month = month(retrieval_date, label = T)) %>%
  filter(region_friendly == "TR") %>% 
  ggplot(aes(x = month, y = spat_count_region, group = year, shape = spat_count_region > 0)) +
  geom_point() +
  geom_errorbar(aes(ymin = spat_count_region - spat_count_se, ymax = spat_count_region + spat_count_se)) +
  facet_wrap(~year, ncol = 1) +
  theme_classic()