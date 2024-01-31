# models for spatial analysis
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '00_vis-custom.R'))
source(here('R', '01_load-tidy-spat.R'))

# glimpse(spat)

# generalized linear models -----------------------------------------------
spat <- spat %>% mutate(region = as.character(region_friendly)) # make region just a character variable, rather than ordered

# region
mod.0 <- glmmTMB::glmmTMB(spat_std ~ region, data = spat, family = "nbinom2")
summary(mod.0)

# region + year
mod.1 <- update(mod.0,  . ~. + year)
summary(mod.1)

# region*year
mod.2 <- update(mod.1, .~. + region:year)
summary(mod.2)

# year
mod.3 <- glmmTMB::glmmTMB(spat_std ~ year, data = spat, family = "nbinom2")
summary(mod.3)

# check out the data
ggplot(spat, aes(x = region_friendly, y = spat_std)) +
  facet_wrap(~year) +
  geom_point(position = "jitter") +
  geom_point(color = "red")

# merge models into one list
modset = list(mod.0,mod.1,mod.2,mod.3)
modnames2 = c("region", "region + year", "region * year", "year")
names(modset) <- modnames2

#AIC(c) table of all models
# create model selection table
bbmle::AICctab(modset, weights = TRUE)

AICcmodavg::aictab(modset, modnames2, second.ord = FALSE) #model selection table with AIC
AICcmodavg::aictab(modset, modnames2, second.ord = TRUE) #model selection table with AICc

#check autocorrelation issues
res1 <- simulateResiduals(mod.2)
plot(res1)
agg.res1 = recalculateResiduals(res1,group=spat$year)
time = unique(spat$year)
plot(time,agg.res1$scaledResiduals,pch=16)
