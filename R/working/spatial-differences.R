
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
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "Region",
       y = "Mean Standardized Spat Per Region")


# create overall plot over time -------------------------------------------

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
  labs(x = "", y = "Mean Standardized Spat Per Region")


# create a plot for each site over time -----------------------------------

dat_soak %>% 
  mutate(year = as.character(year(soak_month)),
         month = month(soak_month, label = T)) %>%
  filter(region_friendly == "TR") %>% 
  ggplot(aes(x = month, y = spat_std_count_region, group = year, color = spat_std_count_region > 0)) +
  geom_point() +
  geom_errorbar(aes(ymin = spat_std_count_region - spat_std_count_se, ymax = spat_std_count_region + spat_std_count_se)) +
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  theme_classic() +
  scale_color_manual(values = c('gray', 'black')) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Standardized Spat Per Month",
       title = "TR: Tolomoto River")

dat_soak %>% 
  mutate(year = as.character(year(soak_month)),
         month = month(soak_month, label = T)) %>%
  filter(region_friendly == "TR") %>% 
  ggplot(aes(x = month, y = spat_std_count_region, group = year, shape = spat_std_count_region > 0, color = year)) +
  geom_point(size = 3) +
  theme_classic() +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "",
       y = "Mean Standardized Spat Per Month",
       title = "TR: Tolomato River")



# generalized linear model ------------------------------------------------

# syntax: glm(y ~ x, data = data, family = "gaussian")
# Generalized linear model (GLM) is a generalization of ordinary linear regression 
# that allows for response variables that have error distribution models other than 
# a normal distribution like Gaussian distribution.

# https://bookdown.org/steve_midway/DAR/glms-generalized-linear-models.html

mod.0 <- glmmTMB::glmmTMB(spat_std_count_region ~ region_friendly, data = dat_soak, family = "nbinom2")
summary(mod.0)

mod.1 <- update(mod.0, . ~. + soak_month)
 

mod.2 <- update(mod.1, . ~. + soak_month:region_friendly)
summary(mod.2)

modset = list(mod.0,mod.1,mod.2)
modnames2 = c("region_friendly", "region_friendly + soak_month", "soak_month * region_friendly")
names(modset) <- modnames2

#AIC(c) table of all models
# create model selection table
bbmle::AICctab(modset, weights = TRUE)

AICcmodavg::aictab(modset, modnames2, second.ord = FALSE) #model selection table with AIC
AICcmodavg::aictab(modset, modnames2, second.ord = TRUE) #model selection table with AICc

# create scaled residuals simulated from the fitted model
res1 <- simulateResiduals(mod.1)
plot(res1)


# # glm ---------------------------------------------------------------------
# 
# mod.0 <- glm(spat_std_count_region ~ region_friendly, data = dat_soak, family = "poisson")
# summary(mod.0)
# 
# mod.1 <- update(mod.0, . ~. + soak_month)
# summary(mod.1)
# 
# mod.2 <- update(mod.1, . ~. + soak_month:region_friendly)
# summary(mod.2)