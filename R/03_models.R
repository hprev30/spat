# models for spatial analysis
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))

# glimpse(spat)

# generalized linear models -----------------------------------------------
spat2 <- spat %>% mutate(year = factor(year)) %>% rename(region = region_friendly) # make year a factor and rename region to shorten

# intercept 0 plus log link of soak time days
mod.0 <- glmmTMB::glmmTMB(spat_count ~ offset(log(soak_time_days)), data = spat2, family = "nbinom2")
summary(mod.0)

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

# compare models
anova(mod.0, mod.1, mod.2, mod.3, mod.4)

# model diagnostics -------------------------------------------------------

# test for autocorrelation on full model
res1 <- simulateResiduals(mod.2)
plot(res1)
performance::check_autocorrelation(mod.2)

# calculating aggregated residuals per group
simulationOutput1 = recalculateResiduals(res1, group = spat2$region)
plot(simulationOutput1)

simulationOutput2 = recalculateResiduals(res1, group = spat2$year)
plot(simulationOutput2)

# check for overdispersion
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


m.year <- emmeans(mod.2, ~ year)
m.year

pairs(m.year, adjust = "tukey")

plot(m.year, comparisons = T)


# plots -------------------------------------------------------------------


# check out the data
ggplot(spat2, aes(x = region, y = spat_count)) +
  facet_wrap(~year) +
  geom_point(position = "jitter") +
  geom_point(color = "red") +
  theme_bw()

