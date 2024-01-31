# preparing water quality data

# 00 load packages --------------------------------------------------------
# library(here)
# source(here('R', '00_load-packages.R'))
# source(here('R', '00_vis-custom.R'))


# 01 load swmp WQ data ----------------------------------------------------
# load data, qaqc to remove suspect or rejected values
# aggregate data to monthly medians for temp, sal, turb

fun_in <- function(x) median(x, na.rm = TRUE)

pi <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmpiwq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", FUN = fun_in, params = c('temp', 'sal', 'turb'))


ss <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmsswq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", FUN = fun_in, params = c('temp', 'sal', 'turb'))


fm <- SWMPr::import_local(here('data', 'swmp','59698.zip'), 
                          station_code = 'gtmfmwq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) %>% 
  SWMPr::aggreswmp(by = "months", FUN = fun_in, params = c('temp', 'sal', 'turb'))


# 02 load swmp NUT data ---------------------------------------------------

## 02.1 load 2002-2023 Nutrient Data ----

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

## 02.2 wrangle data for merging ----

NUT <- NUT %>% filter(!is.na(rep)) # remove "S" reps in dataset

## 02.3 wrangle NUT to swmpr for better filtering ----
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

## 02.4 qaqc swmpr -----

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

# 03 merge NUT with WQ files and region_friendly --------------------------

TR <- pi %>% left_join(pi_chla, by = "datetimestamp") %>% mutate(region_friendly = "TR")
GR <- pi %>% left_join(pi_chla, by = "datetimestamp") %>% mutate(region_friendly = "GR")

SA <- ss %>% left_join(ss_chla, by = "datetimestamp") %>% mutate(region_friendly = "SA")
SR <- ss %>% left_join(ss_chla, by = "datetimestamp") %>% mutate(region_friendly = "SR")

FM <- fm %>% left_join(fm_chla, by = "datetimestamp") %>% mutate(region_friendly = "FM")


# 04 combine into one df --------------------------------------------------

wq <- bind_rows(TR, GR, SA, SR, FM) %>% 
  mutate(region_friendly = factor(region_friendly, 
                                  levels = c('TR',
                                             'GR', 
                                             'SA',
                                             'SR',
                                             'FM'))) %>% 
  rename(soak_month = datetimestamp)


# 05 clean up all dfs -----------------------------------------------------

rm(fm, FM, fm_chla,
   GR, NUT, pi, pi_chla,
   SA, SR, ss, ss_chla,
   TR)
