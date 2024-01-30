# 00 load packages --------------------------------------------------------
# library(here)
# source(here('R', '00_load-packages.R'))

# 01 load spat data file --------------------------------------------------

spat_dat <- readxl::read_xlsx(here('data','spat_data.xlsx'), sheet = 'GTM') %>% 
              janitor::clean_names() %>% # clean variable names
                select(1:25) # remove extra columns present in excel file



# 02 assess data ----------------------------------------------------------

# glimpse(spat_dat) # look at variables and their classes
# 
# unique(spat_dat$region) # look at unique regions present
# # ! found that there are "NAs" in the regions variable.
# 
# filter(spat_dat, is.na(region)) # looked at the NAs - will need to remove these

# regions are present in full name, provide abbreviation?
region <- tribble(
                  ~region, ~region_friendly,
                  'Fort Matanzas', 'FM',
                  'Guana River', 'GR',
                  'Saint Augustine', 'SA',
                  'Salt Run', 'SR',
                  'Tolomato River', 'TR'
                )


# 03 wrangle data ---------------------------------------------------------

# plan to be wrangle and tidied:

  # generic `spat` dataframe:
    # select: year, month, soak time, regions, reef name, stringer, adj_spat, qaqc
    # filter: qaqc to keep only '<0>', remove na regions
    # mutate: convert year and month into a Date object
    # join: with regions tribble to have region_friendly acronymns
    # select(-qa_qc_code, region) 
    # mutate: create ordered factor for region
  
spat <- spat_dat %>% 
  select(year, month, soak_time_days, region, reef_name, stringer, adj_spat, qa_qc_code) %>%
  dplyr::filter(grepl("<0>", qa_qc_code) & !is.na(region)) %>% 
  mutate(soak_month = lubridate::ymd(paste0(year, "-", month, "-01"))) %>% 
  left_join(region, by = 'region') %>% 
  select(-region, -qa_qc_code) %>% 
  mutate(region_friendly = factor(region_friendly, 
                                  levels = c('TR',
                                             'GR', 
                                             'SA',
                                             'SR',
                                             'FM')))

rm(region) # get rid of the region shortened tribble

  # `soakdays` dataframe
    # select: retrieval_date, tag_name, region_friendly, soak_time_days
    # remove duplicates: dplyr::distinct()

soakdays <- spat %>% 
    select(soak_month, reef_name, region_friendly, soak_time_days) %>% 
    dplyr::distinct()


# 04 tidy data ------------------------------------------------------------

  # `dat` dataframe for spat analysis
    # group_by: soak_month and region_friendly and reef_name
    # summarize: spat_count = sum(adj_spat, na.rm = T) to get total spat counted on each tree (e.g. totaling the stringers)
    # group_by: soak_month and region_friendly
    # summarize and round: spat_count_region = mean(spat_count, na.rm = T) to get average 
    # 
# to standardize by soak days
    # left_join with soak days by retrieval_date, region_friendly, tag_name # to be able to bring back in soak_time_days
    # mutate: spat_std = spat_count/soak_time_days # to get a new variable
    # group_by: retrieval_date and region_friendly
    # summarize: spat_count_region = mean(spat_count, na.rm = T), 
    #             spat_count_sd = sd(spat_count, na.rm = T), 
    #               spat_count_se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count))) 

dat <- spat %>% 
  group_by(soak_month, region_friendly, reef_name) %>% 
  summarize(spat_count = sum(adj_spat, na.rm = T)) %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(spat_count_region = ceiling(mean(spat_count, na.rm = T)),
            spat_count_sd = ceiling(sd(spat_count, na.rm = T)),
            spat_count_se = ceiling((sd(spat_count, na.rm = T)/sqrt(length(spat_count))))
            ) %>% 
  ungroup()

dat_soak <-  spat %>% 
  group_by(soak_month, region_friendly, reef_name) %>% 
  summarize(spat_count = sum(adj_spat, na.rm = T)) %>%  
  left_join(soakdays, by = c('soak_month', 'region_friendly', 'reef_name')) %>% 
  mutate(spat_std = spat_count/soak_time_days) %>% 
  group_by(soak_month, region_friendly) %>% 
  summarise(spat_std_count_region = ceiling(mean(spat_std, na.rm = T)),
            spat_std_count_sd = ceiling(sd(spat_std, na.rm = T)),
            spat_std_count_se = ceiling((sd(spat_std, na.rm = T)/sqrt(length(spat_std))))
            ) %>% 
  ungroup()


rm(soakdays) # remove soakdays dataframe   


# 05 create basic summary stat tables -------------------------------------

