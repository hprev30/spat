# Summary table information for study

# 00 load packages and data -----------------------------------------------
library(here)
source(here('R', '00_load-packages.R'))
source(here('R', '01_load-tidy-spat.R'))


# 01 create basic summary stat tables -------------------------------------

spat %>% 
  split(.$region_friendly) %>% 
  purrr::map(summary)