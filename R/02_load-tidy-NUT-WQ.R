# preparing water quality data

# 00 load packages --------------------------------------------------------
# library(here)
# source(here('R', '00_load-packages.R'))
# source(here('R', '00_vis-custom.R'))


# 01 load swmp WQ data ----------------------------------------------------
# load data, qaqc to remove suspect or rejected values
# aggregate data to monthly medians for temp, sal, turb

pi <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmpiwq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

ss <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmsswq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))  

fm <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmfmwq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

# 02 load swmp NUT data ---------------------------------------------------

## 02.1 load 2002-2023 Nutrient Data 

nms <- names(read_excel(here::here('data',
                                   'swmp',
                                   'gtmnut2002-2023_QC_zeros-corrected.xlsx'), 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

NUT <- readxl::read_xlsx(here::here('data',
                                    'swmp',
                                    'gtmnut2002-2023_QC_zeros-corrected.xlsx'),
                         col_types = c("text", 
                                       "date", 
                                       "numeric", 
                                       "numeric", 
                                       "text", 
                                       class2)) %>% # specify how to read in these columns
  janitor::clean_names()

# clean environment
rm(nms, class, class2)

## 02.2 wrangle data for merging 

NUT <- NUT %>% filter(!is.na(rep)) # remove "S" reps in dataset

## 02.3 wrangle NUT to swmpr for better filtering 
# The `swmpr()` call needs to have just datetimestamp and data+qa columns, so remove the extras, while also making names lower case.

timezone <- "America/Jamaica" # needs a timezone

stations <- c("gtmpinut", "gtmssnut", "gtmfmnut", "gtmpcnut")

for (i in 1:length(stations)){
  
  tempdf <- swmpr(as.data.frame(NUT %>%
                                  filter(station_code == stations[i]) %>%
                                  select(-station_code) %>%
                                  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                                                      tz = timezone,
                                                                      format = '%m/%d/%Y %H:%M')) %>%
                                  rename(datetimestamp = date_time_stamp,
                                         unc_chla_n = unc_ch_la_n,
                                         f_unc_chla_n = f_unc_ch_la_n) %>%
                                  filter(monitoring_program == 1) %>%
                                  select(-monitoring_program, -rep)), 
                  stations[i])
  
  # 
  name <- attr(tempdf, "station") # pull out the name you want of the file
  # 
  assign(paste0("swmp", "_", name), tempdf)
  
  rm(tempdf, name, i)
}

# check object(s) to confirm they are swmpr objects
# class(swmp_gtmpcnut)
# str(swmp_gtmpcnut)

rm(timezone, stations)

## 02.4 qaqc swmpr 

# use the qaqc functions on the data
# aggregate to monthly for chla
# filter for only spat timeframe

pi_chla <- swmp_gtmpinut %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", params = "chla_n") %>% 
  dplyr::filter(datetimestamp > as.Date('2015-01-31'))

ss_chla <- swmp_gtmssnut %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", params = "chla_n") %>% 
  dplyr::filter(datetimestamp > as.Date('2015-01-31'))

fm_chla <- swmp_gtmfmnut %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", params = "chla_n") %>% 
  dplyr::filter(datetimestamp > as.Date('2015-01-31'))

# remove unfiltered data objects
rm(swmp_gtmpinut, 
   swmp_gtmssnut, 
   swmp_gtmfmnut,
   swmp_gtmpcnut)

# 03 wq stats by station --------------------------------------------------

# im sure there is a way to make this all into one nice beautiful function, 
# but I just don't have the patience for it right now
fun_max <- function(x) max(x, na.rm = TRUE)
fun_min <- function(x) min(x, na.rm = TRUE)
fun_se <- function(x) (sd(x, na.rm = TRUE)/sqrt(length(x)))
fun_med <- function(x) median(x, na.rm = TRUE)

  max <- pi %>% SWMPr::aggreswmp(by = "months", FUN = fun_max, 
                                    params = c('temp', 'sal', 'turb')) %>% 
    rename(temp_max = temp,
           sal_max = sal,
           turb_max = turb)
  min <- pi %>% SWMPr::aggreswmp(by = "months", FUN = fun_min, 
                                    params = c('temp', 'sal', 'turb')) %>% 
    rename(temp_min = temp,
           sal_min = sal,
           turb_min = turb)
  mean <- pi %>% SWMPr::aggreswmp(by = "months", 
                                     params = c('temp', 'sal', 'turb')) %>% 
    rename(temp_mean = temp,
           sal_mean = sal,
           turb_mean = turb)
  se <- pi %>% SWMPr::aggreswmp(by = "months", FUN = fun_se, 
                                   params = c('temp', 'sal', 'turb')) %>% 
    rename(temp_se = temp,
           sal_se = sal,
           turb_se = turb) 

pi_dat <- max %>% 
  left_join(min, by = "datetimestamp") %>% 
  left_join(mean, by = "datetimestamp") %>% 
  left_join(se, by = "datetimestamp") %>% 
  mutate(station = "piwq")

rm(max, mean, min, se)

####

max <- ss %>% SWMPr::aggreswmp(by = "months", FUN = fun_max, 
                               params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_max = temp,
         sal_max = sal,
         turb_max = turb)
min <- ss %>% SWMPr::aggreswmp(by = "months", FUN = fun_min, 
                               params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_min = temp,
         sal_min = sal,
         turb_min = turb)
mean <- ss %>% SWMPr::aggreswmp(by = "months", 
                                params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_mean = temp,
         sal_mean = sal,
         turb_mean = turb)
se <- ss %>% SWMPr::aggreswmp(by = "months", FUN = fun_se, 
                              params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_se = temp,
         sal_se = sal,
         turb_se = turb) 

ss_dat <- max %>% 
  left_join(min, by = "datetimestamp") %>% 
  left_join(mean, by = "datetimestamp") %>% 
  left_join(se, by = "datetimestamp") %>% 
  mutate(station = "sswq")

rm(max, mean, min, se)

###

max <- fm %>% SWMPr::aggreswmp(by = "months", FUN = fun_max, 
                               params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_max = temp,
         sal_max = sal,
         turb_max = turb)
min <- fm %>% SWMPr::aggreswmp(by = "months", FUN = fun_min, 
                               params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_min = temp,
         sal_min = sal,
         turb_min = turb)
mean <- fm %>% SWMPr::aggreswmp(by = "months", 
                                params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_mean = temp,
         sal_mean = sal,
         turb_mean = turb)
se <- fm %>% SWMPr::aggreswmp(by = "months", FUN = fun_se, 
                              params = c('temp', 'sal', 'turb')) %>% 
  rename(temp_se = temp,
         sal_se = sal,
         turb_se = turb) 

fm_dat <- max %>% 
  left_join(min, by = "datetimestamp") %>% 
  left_join(mean, by = "datetimestamp") %>% 
  left_join(se, by = "datetimestamp") %>% 
  mutate(station = "fmwq")

rm(max, mean, min, se)

# 04 merge NUT with WQ files --------------------------

PI <- pi_dat %>% left_join(pi_chla, by = "datetimestamp") %>% rename(date = datetimestamp)
SS <- ss_dat %>% left_join(ss_chla, by = "datetimestamp") %>% rename(date = datetimestamp)
FM <- fm_dat %>% left_join(fm_chla, by = "datetimestamp") %>% rename(date = datetimestamp)

WQ <- bind_rows(PI, SS, FM)

rm(PI, SS, FM,
   pi_dat, ss_dat, fm_dat)

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
  select(datetimestamp, temp, sal, turb) %>% 
  mutate(month = lubridate::month(datetimestamp, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  mutate(station = "piwq")

ss_1 <- ss %>% 
  select(datetimestamp, temp, sal, turb) %>% 
  mutate(month = lubridate::month(datetimestamp, label = T)) %>% 
  left_join(settlement, by = "month") %>% 
  mutate(station = "sswq")

fm_1 <- fm %>% 
  select(datetimestamp, temp, sal, turb) %>% 
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



# extra section -----------------------------------------------------------


