
# Are there spatial differences? ------------------------------------------

glimpse(dat_soak)


# create table of information ---------------------------------------------

dat_soak %>% 
  group_by(region_friendly) %>% 
  summarize(mean = mean(spat_std_count_region, na.rm = T),
            min = min(spat_std_count_region, na.rm = T),
            max = max(spat_std_count_region, na.rm = T),
            sd = sd(spat_std_count_region, na.rm = T), 
            se = (sd(spat_std_count_region, na.rm = T)/sqrt(length(spat_std_count_region)))
  )


# create boxplot for variation --------------------------------------------

dat_soak %>% 
  ggplot(aes(x = region_friendly, y = spat_std_count_region, fill = region_friendly)) +
  geom_boxplot() +
  scale_fill_manual(values = sitecolours) +
  theme_spat() +
  theme(legend.position = "none") +
  labs(x = "Region",
       y = "Mean Spat Per Shell Per Day")
