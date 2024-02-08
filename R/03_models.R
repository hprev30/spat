# models for spatial analysis
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))

# glimpse(spat)

# generalized linear models -----------------------------------------------
spat2 <- spat %>% mutate(year = factor(year)) %>% rename(region = region_friendly) # make year a factor and rename region to shorten

# intercept only
mod.0 <- glmmTMB::glmmTMB(spat_count ~ offset(log(soak_time_days)), data = spat2, family = "nbinom2")
summary(mod)

# region
mod.1 <- update(mod.0, .~. + region)
summary(mod.1)

# region + year
mod.2 <- update(mod.1, . ~. + year)
summary(mod.2)

# region*year
mod.3 <- update(mod.2, .~. + region:year)
summary(mod.3)

# year
mod.4 <- update(mod.0, .~. + year)
summary(mod.4)

# check out the data
ggplot(spat2, aes(x = region, y = spat_std)) +
  facet_wrap(~year) +
  geom_point(position = "jitter") +
  geom_point(color = "red") +
  theme_bw()

means <- spat2 %>% group_by(region) %>% summarize(mean = mean(spat_std, na.rm = T)) 
ggplot(spat2, aes(x = region, y = spat_std, group = region)) +
  geom_point(position = "jitter", color = "gray") +
  geom_point(data = means, aes(x = region, y = mean), color = "black", size = 3) +
  ggrepel::geom_label_repel(data = means, aes(x = region, y = mean, label = round(mean, digits = 2)),
                            nudge_y = 2.5, nudge_x = 0.2) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))


# model selection via AIC -------------------------------------------------

# merge models into one list
modset = list(mod.0,mod.1,mod.2,mod.3, mod.4)
modnames2 = c("intercept", "spat ~ region", "spat ~ region + year", "spat ~ region + year + region * year", "spat ~ year")
names(modset) <- modnames2

#AIC(c) table of all models
# create model selection table

bbmle::AICctab(modset, weights = TRUE)
AICcmodavg::aictab(modset, modnames2, second.ord = TRUE) #model selection table with AICc

performance::check_model(mod.2)

# model diagnostics -------------------------------------------------------

# classic ANOVA style table for output (but uses Chi-Square tests)
car::Anova(mod.2)
# interaction
car::Anova(mod.3)

Anova.glmmTMB(mod.2)

# test for autocorrelation on full model
res1 <- simulateResiduals(mod.2)
plot(res1)
performance::check_autocorrelation(mod.2)

simulationOutput1 = recalculateResiduals(res1, group = spat2$region)
plot(simulationOutput1)

simulationOutput2 = recalculateResiduals(res1, group = spat2$year)
plot(simulationOutput2)



# check for overdispersion
# 
performance::check_overdispersion(mod.2)



# quantify differences ----------------------------------------------------
# estimated marginal means (least-squares means)

m.region <- emmeans(mod.2, ~ region)
m.region

pairs(m.region, adjust = "tukey")
contrast(m.region, method = "pairwise", adjust = "tukey")

plot(m.region, comparisons = T)
# blue bars are the confidence intervals
# red arrows represent a scheme to determine homogeneous groups
# if the red lines overlap for two groups, they are not significantly different using the method chosen


m.year <- emmeans(mod.1, ~ year)
m.year

pairs(m.year, adjust = "tukey")

plot(m.year, comparisons = T)
