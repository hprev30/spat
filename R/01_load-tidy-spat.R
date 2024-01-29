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
    # rename: retrievial_date to retrieval_date
    # select: retrieval_date, soak time, regions, tag name, stringer, adj_spat, qaqc
    # filter: qaqc to keep only '<0>', remove na regions
    # mutate: retrieval date from POSIXct to Date
    # join: with regions tribble to have region_friendly acronymns
    # select(-qa_qc_code, region)
  
spat <- spat_dat %>% 
  rename(retrieval_date = retrievial_date) %>% 
  select(retrieval_date, soak_time_days, region, tag_name, stringer, adj_spat, qa_qc_code) %>%
  dplyr::filter(grepl("<0>", qa_qc_code) & !is.na(region)) %>% 
  mutate(retrieval_date = as.Date(retrieval_date)) %>% 
  left_join(region, by = 'region') %>% 
  select(-region, -qa_qc_code)

rm(region) # get rid of the region shortened tribble

  # `soakdays` dataframe
    # select: retrieval_date, tag_name, region_friendly, soak_time_days
    # remove duplicates: dplyr::distinct()

soakdays <- spat %>% 
    select(retrieval_date, tag_name, region_friendly, soak_time_days) %>% 
    dplyr::distinct()


# 04 tidy data ------------------------------------------------------------

  # `dat` dataframe for spat analysis
    # group_by: retrieval_date and region_friendly and tag_name
    # summarize: spat_count = mean(adj_spat, na.rm = T) to get average spat counted on each tag (e.g. averaging the stringers)
    # left_join with soak days by retrieval_date, region_friendly, tag_name # to be able to bring back in soak_time_days
    # mutate: spat_std = spat_count/soak_time_days # to get a new variable
    # group_by: retrieval_date and region_friendly
    # summarize: spat_count_region = mean(spat_count, na.rm = T), 
    #             spat_count_sd = sd(spat_count, na.rm = T), 
    #               spat_count_se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count))) 

dat <- spat %>% 
  group_by(retrieval_date, region_friendly, tag_name) %>% 
  summarize(spat_count = mean(adj_spat, na.rm = T)) %>% 
  group_by(retrieval_date, region_friendly) %>% 
  summarise(spat_count_region = mean(spat_count, na.rm = T),
            spat_count_sd = sd(spat_count, na.rm = T),
            spat_count_se = (sd(spat_count, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  ungroup()

dat_soak <-  spat %>% 
  group_by(retrieval_date, region_friendly, tag_name) %>% 
  summarize(spat_count = mean(adj_spat, na.rm = T)) %>% 
  left_join(soakdays, by = c('retrieval_date', 'region_friendly', 'tag_name')) %>% 
  mutate(spat_std = spat_count/soak_time_days) %>% 
  group_by(retrieval_date, region_friendly) %>% 
  summarise(spat_std_count_region = mean(spat_std, na.rm = T),
            spat_std_count_sd = sd(spat_std, na.rm = T),
            spat_std_count_se = (sd(spat_std, na.rm = T)/sqrt(length(spat_count)))
  ) %>% 
  ungroup()

rm(soakdays) # remove soakdays dataframe   

