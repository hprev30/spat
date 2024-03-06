# Summary statistics of water quality parameters inside and outside of spat settlement periods

# library(here)
# source(here('R', '00_load-packages.R'))
# source(here('R', '00_vis-custom.R'))
# source(here('R', '02_load-tidy-NUT-WQ.R'))


# 05 settlement periods ---------------------------------------------------

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

pi_1 <- pi %>% 
  left_join(pi_chla, by = "datetimestamp") %>% 
  select(datetimestamp, temp, sal, turb, chla_n) %>% 
  mutate(month = lubridate::month(datetimestamp, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  mutate(station = "piwq")

ss_1 <- ss %>% 
  left_join(ss_chla, by = "datetimestamp") %>% 
  select(datetimestamp, temp, sal, turb, chla_n) %>%
  mutate(month = lubridate::month(datetimestamp, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  mutate(station = "sswq")

fm_1 <- fm %>% 
  left_join(fm_chla, by = "datetimestamp") %>% 
  select(datetimestamp, temp, sal, turb, chla_n) %>%
  mutate(month = lubridate::month(datetimestamp, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  mutate(station = "fmwq")

# settlement_WQ <- bind_rows(pi_1, ss_1, fm_1) %>% 
#   group_by(station, settlement) 

pi_1 %>% 
  split(.$settlement) %>% 
  purrr::map(summary)

ss_1 %>% 
  split(.$settlement) %>% 
  purrr::map(summary)

fm_1 %>% 
  split(.$settlement) %>% 
  purrr::map(summary)

# standard errors

fun_se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))

pi_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise(temp = fun_se(temp),
            sal = fun_se(sal),
            turb = fun_se(turb),
            chl = fun_se(chla_n))

ss_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise(temp = fun_se(temp),
            sal = fun_se(sal),
            turb = fun_se(turb),
            chl = fun_se(chla_n))

fm_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise(temp = fun_se(temp),
            sal = fun_se(sal),
            turb = fun_se(turb),
            chl = fun_se(chla_n))
# extra section -----------------------------------------------------------

# standard dev

fun_se <- function(x) (sd(x, na.rm = T)/sqrt(length(x)))

pi_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise_all(list(~ mean(., na.rm = T), ~ sd(., na.rm = T)))

ss_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise_all(list(~ mean(., na.rm = T), ~ sd(., na.rm = T)))

fm_1 %>% 
  select(settlement, temp, sal, turb, chla_n) %>% 
  group_by(settlement) %>% 
  summarise_all(list(~ mean(., na.rm = T), ~ sd(., na.rm = T)))

