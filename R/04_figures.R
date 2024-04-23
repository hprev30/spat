# graphs for manuscript
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))
source(here('R', '02_load-tidy-NUT-WQ.R'))

# spat over time ----------------------------------------------------------
a <- spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>%
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se)) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_classic(base_family = "serif") +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell")

ggsave(a, filename = here('output', 'spat-over-time.png'), dpi = 300, units = "in", width = 7, height = 3)


b <- spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>%
  filter(soak_month > as.Date('2015-01-01') & soak_month < as.Date('2017-01-01')) %>% 
  ggplot(aes(x = soak_month, y = mean)) +
  geom_ribbon(aes(xmin = as.Date('2016-08-15'), xmax = as.Date('2016-10-15')),
              fill = "gray90") +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_line(aes(group = region_friendly, color = region_friendly)) +
  geom_point(aes(, group = region_friendly, color = region_friendly)) +
  # geom_errorbar(aes(ymin = mean - se, ymax = mean + se), alpha = 0.7) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  # scale_y_continuous(limits = c(0,450)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_bw(base_family = "serif") +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-10-15"), 
           y = 60)

c <- spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>%
  filter(soak_month > as.Date('2017-01-01') & soak_month < as.Date('2019-01-01')) %>% 
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin = mean - se, ymax = mean + se), alpha = 0.7) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  # scale_y_continuous(limits = c(0,450)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_bw(base_family = "serif") +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell") +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-09-20"), 
           y = 120)

d <- spat %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(mean = mean(spat_count, na.rm = T),
            se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>%
  filter(soak_month > as.Date('2019-01-01') & soak_month < as.Date('2021-01-01')) %>% 
  ggplot(aes(x = soak_month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin = mean - se, ymax = mean + se), alpha = 0.7) +
  scale_colour_manual(name = "Region", values = sitecolours) +
  # scale_y_continuous(limits = c(0,450)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", date_minor_breaks = "months") +
  theme_bw(base_family = "serif") +
  theme(legend.position = "top",
        axis.text = element_text(color = "black")) +
  labs(x = "", y = "Mean Spat Per Shell") +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-09-15"), 
           y = 300)

fig3 <- 
(b + theme(legend.position = "none") + labs(y = "")) / 
  (c + theme(legend.position = "none")) / 
  (d + theme(legend.position = "bottom") + labs(y = ""))

ggsave(fig3, filename = here('output', 'figure3-2.png'), 
       dpi = 600, 
       units = "in", 
       height = 6.08,
       width = 5.5)


# spat by region with stats -----------------------------------------------

means <- spat %>% group_by(region_friendly) %>% summarize(mean = mean(spat_count, na.rm = T)) 
a <- spat %>% 
  ggplot(aes(x = region_friendly, y = spat_count, color = region_friendly)) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
  geom_point(data = means, aes(x = region_friendly, y = mean), color = "black", size = 4) +
  scale_color_manual(values = sitecolours) +
  ggrepel::geom_label_repel(data = means, aes(x = region_friendly, y = mean, label = round(mean, digits = 2),
                                              family = "serif"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none") +
  labs(x = "Region", y = "Mean Spat Per Shell") +
  annotate("text",
           label = "A",
           x = 'TR',
           y = 500,
           family = "serif",
           fontface = 2) +
  annotate("text",
           label = "B", 
           x = "GR",
           y = 400,
           family = "serif",
           fontface = 2) +
  annotate("text",
           label = "B",
           x = "SA",
           y = 400,
           family = "serif",
           fontface = 2) +
  annotate("text",
           label = "C",
           x = "SR",
           y = 300,
           family = "serif",
           fontface = 2) +
  annotate("text",
           label = "C",
           x = "FM",
           y = 300,
           family = "serif",
           fontface = 2)



# spat by year with stats -------------------------------------------------

means2 <- spat %>% group_by(year) %>% summarize(mean = mean(spat_count, na.rm = T)) 
b <- spat %>% 
  ggplot(aes(x = year, y = spat_count)) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 2, color = "gray50") +
  geom_point(data = means2, aes(x = year, y = mean), color = "black", size = 4) +
  ggrepel::geom_label_repel(data = means2, aes(x = year, y = mean, label = round(mean, digits = 2),
                                               family = "serif"), size = 3.3,
                            nudge_y = 2.5, nudge_x = 0.2, color = "black") +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none") +
  labs(x = "Year", y = "Mean Spat Per Shell") +
  annotate("text",
           label = "A",
           x = 2015,
           y = 200,
           family = "serif",
           fontface = "bold") +
  annotate("text",
           label = "A",
           x = 2016,
           y = 200,
           family = "serif",
           fontface = "bold") +
  annotate("text",
           label = "B",
           x = 2017,
           y = 300,
           family = "serif",
           fontface = "bold") +
  annotate("text",
           label = "B",
           x = 2018,
           y = 300,
           family = "serif",
           fontface = "bold") +
  annotate("text",
           label = "C",
           x = 2019,
           y = 500,
           family = "serif",
           fontface = "bold") +
  annotate("text",
           label = "C",
           x = 2020,
           y = 500,
           family = "serif",
           fontface = "bold")




# combine spat by region and year into one side-by-side plot --------------

fig4 <-
(a + labs(title = "A")) + (b + labs(y = "", title = "B"))

ggsave(fig4, filename = here('output', 'figure4.png'),
       dpi = 600, units = "in",
       width = 7, height = 3.5)

# settlement period -------------------------------------------------------

fig5 <- 
spat %>% 
  mutate(month = month(soak_month, label = T)) %>% 
  group_by(region_friendly, month)  %>% 
  summarize(mean = mean(spat_count, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean, group = region_friendly, color = region_friendly)) +
  geom_line() +
  # geom_point() +
  scale_color_manual(name = "Region", values = sitecolours) +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top") +
  labs(x = "", y = "Mean Spat Per Shell") +
  geom_segment(aes(x = "Apr", y = 200, xend = "Oct", yend = 200), 
               color = "black", size = 2, linetype = "dashed") +
  annotate("text",
           label = "Settlement Period",
           x = "Jul",
           y = 180,
           family = "serif")

ggsave(fig5, filename = here('output', 'figure5.png'),
       dpi = 600, units = "in", 
       width = 5.5, height = 3)

# monthly water quality parameters over time ------------------------------

wq_a <- 
WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  # geom_ribbon(aes(ymin = temp_min, ymax = temp_max), alpha = 0.3) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = temp_mean)) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Temperature (\u00b0C)",
       title = "A") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 30) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 30) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 30)

wq_b <- 
WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = sal_mean)) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Salinity (psu)",
       title = "B") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 37) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 37) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 37)

wq_c <-
WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  # geom_ribbon(aes(ymin = temp_min, ymax = temp_max), alpha = 0.3) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = turb_mean)) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(name = "Station", values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Turbidity (NTU)",
       title = "C") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 24) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 24) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 24)

wq_d <-
WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  # geom_ribbon(aes(ymin = temp_min, ymax = temp_max), alpha = 0.3) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = chla_n), size = 0.7) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(name = "Station", values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Chlorophyll a (\U00B5g/L)",
       title = "D") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 15) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 15) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 15)

fig6 <-
(wq_a + wq_b) / (wq_c + wq_d)

ggsave(fig6, filename = here('output', 'figure6-2.png'),
       dpi = 600, units = "in",
       height = 4.5, width = 7)

# special call out

wq_a2 <- 
  WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = sal_min)) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Monthly Minimum\nSalinity (psu)",
       title = "A") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 37) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 37) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 37)
wq_b2 <-
  WQ %>% 
  ggplot(aes(x = date, group = station, color = station)) +
  # geom_ribbon(aes(ymin = temp_min, ymax = temp_max), alpha = 0.3) +
  geom_vline(xintercept = as.Date("2016-10-07"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-09-11"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-09-04"), linetype = "dashed") +
  geom_line(aes(y = turb_max)) +
  theme_bw(base_family = "serif") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_color_manual(name = "Station", values = stationcolours, labels = c("FM", "PI", "SS")) +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
  labs(x = "", 
       y = "Monthly Maximum\nTurbidity (NTU)",
       title = "B") +
  annotate("text",
           label = "a",
           family = "serif",
           x = as.Date("2016-12-01"),
           y = 320) +
  annotate("text",
           label = "b",
           family = "serif",
           x = as.Date("2017-11-01"),
           y = 320) +
  annotate("text",
           label = "c",
           family = "serif",
           x = as.Date("2019-11-01"),
           y = 320)

fig7 <-
wq_a2 / wq_b2

ggsave(fig7, filename = here('output', 'figure7-2.png'),
       dpi = 600, units = "in",
       width = 5.5, height = 5)
