# Summary table information for study

# 00 load packages and data -----------------------------------------------
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '01_load-tidy-spat.R'))


# 01 create basic summary stat tables -------------------------------------

spat %>% 
  split(.$region_friendly) %>% 
  purrr::map(summary)


# 02 spat and settlement periods ------------------------------------------

settlement <- tribble(
  ~month, ~settlement,
  'Jan', 'None',
  'Feb','None',
  'Mar', 'None',
  'Apr', 'Yes',
  'May', 'Yes',
  'Jun', 'Yes', 
  'Jul', 'Yes',
  'Aug', 'Yes', 
  'Sep', 'Yes', 
  'Oct', 'Yes',
  'Nov', 'None',
  'Dec', 'None'
)

# average spat per shell during settlement period and outside
spat %>% 
  mutate(month = month(soak_month, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  group_by(region_friendly, settlement) %>% 
 summarize(mean = ceiling(mean(spat_count, na.rm = T)),
           se = ceiling((sd(spat_count, na.rm = T)/sqrt(length(spat_count)))))

# average total settlement per shell during settlement period and outside per year
spat %>% 
  mutate(month = month(soak_month, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  group_by(year, region_friendly, settlement) %>% 
  summarize(total = sum(spat_count, na.rm = T)) %>% 
  group_by(region_friendly, settlement) %>% 
  summarize(mean_total = ceiling(mean(total, na.rm = T)),
            se = ceiling((sd(total, na.rm = T)/sqrt(length(total)))))

rm(settlement)


# summary -----------------------------------------------------------------


