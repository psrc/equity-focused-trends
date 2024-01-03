# Hardcoded functions to gather and format PUMS tables
# To run this script successfully, install github version of tidycensus: devtools::install_github("walkerke/tidycensus")

library(psrccensus)
library(tidyverse)
library(tidycensus)

# Available:
# 2021 1 year data
# 2020 5 year data
# 2022 1 year data

# Not available:
# 2020 1 year data

# DIS
# 1 - with a disability
# 2 - without a disability

create_modsect_pums_table <- function(span, dyear) {
  
  pums <- get_psrc_pums(span = span,                                # Denoting ACS 5-year estimates; 1-year also available
                        dyear = dyear,                              # Last data year of span
                        level = "p",                                # Unit of analysis == household ("p" used for person)
                        vars = c("LUM_JOBSECTOR",                   # Modeling Sectors
                                 "DIS",                              # Disability recode
                                 "ESR",                                         # Employment status recode
                                 "AGEP"                                         # Age
                        ))  
  
  # filter for Civilian/Armed persons and between 18-64 years
  pums <- pums %>% filter((grepl("^(Civilian|Armed) ", as.character(ESR)) & between(AGEP, 18,64)))
  ## uncomment this line if using CRAN tidycensus
  # pums <- pums %>% filter(ESR %in% c(1,2,4,5) & between(AGEP, 18,64))
  
  df <- psrc_pums_count(pums, 
                        group_vars = c("LUM_JOBSECTOR",       # Modeling Sectors
                                       "DIS"                  # Disability recode
                        )) 
  
  ## uncomment these lines if using CRAN tidycensus
  # if(dyear == 2022) {
  #   df <- df %>% 
  #     mutate(DIS = as.character(DIS)) %>% 
  #     mutate(DIS = case_match(DIS,
  #                             "1" ~ "With a disability",
  #                             "2" ~ "Without a disability",
  #                             .default = DIS))
  # }
  
  df_sum <- df %>%
    filter(DIS != 'Total') %>%
    group_by(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS) %>%
    summarise(count = sum(count), count_moe = moe_sum(estimate = count, moe = count_moe)) %>%
    mutate(se = count_moe/1.645) %>% 
    mutate(cv = (se/count)) 
  
  denom <- df %>%
    filter(LUM_JOBSECTOR == 'Total') %>%
    select(COUNTY, count_denom = count, moe_denom = count_moe)
  
  df_join <- df_sum %>%
    left_join(denom, by = 'COUNTY') %>%
    group_by(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS) %>%
    summarise(count, count_moe, cv, share = count/count_denom, share_moe = moe_prop(count, count_denom, count_moe, moe_denom))
  
  d <- df_join %>%
    select(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS, count, count_moe, cv,share, share_moe)
}

# test_lumod <- create_modsect_pums_table(span = 1, dyear = 2022)
# test_lumod %>%
#   # filter(DIS == 'With a disability' & !is.na(LUM_JOBSECTOR)) %>%
#   filter(DIS == 'With a disability') %>%
#   ungroup() %>%
#   summarise(count = sum(count))

create_covempsect_pums_table <- function(span, dyear) {
  
  pums <- get_psrc_pums(span = span,                                            # Denoting ACS 5-year estimates; 1-year also available
                        dyear = dyear,                                          # Last data year of span
                        level = "p",                                            # Unit of analysis == household ("p" used for person)
                        vars = c("STANDARD_JOBSECTOR",                          # Cov Emp Sectors
                                 "DIS",                                         # Disability recode
                                 "ESR",                                         # Employment status recode
                                 "AGEP"                                         # Age
                        ))   
  
  # filter for Civilian/Armed persons and between 18-64 years
  pums <- pums %>% filter((grepl("^(Civilian|Armed) ", as.character(ESR)) & between(AGEP, 18,64)))
  ## uncomment this line if using CRAN tidycensus
  # pums <- pums %>% filter(ESR %in% c(1,2,4,5) & between(AGEP, 18,64))
  
  df <- psrc_pums_count(pums, 
                        group_vars = c("STANDARD_JOBSECTOR",       
                                       "DIS"                  
                        )) %>% 
    mutate(acs_type = paste0('acs', span))
  
  ## uncomment these lines if using CRAN tidycensus
  # if(dyear == 2022) {
  #   df <- df %>% 
  #     mutate(DIS = as.character(DIS)) %>% 
  #     mutate(DIS = case_match(DIS,
  #                             "1" ~ "With a disability",
  #                             "2" ~ "Without a disability",
  #                             .default = DIS))
  # }
  
  denom <- df %>% 
    filter(STANDARD_JOBSECTOR == 'Total') %>%
    select(COUNTY, count_denom = count, moe_denom = count_moe)
  
  df_sum <- df %>%
    filter(DIS != 'Total') %>%
    mutate(se = count_moe/1.645) %>%
    mutate(cv = (se/count))
  
  d <- df_sum %>%
    left_join(denom, by = 'COUNTY') %>%
    group_by(acs_type, DATA_YEAR, COUNTY, STANDARD_JOBSECTOR, DIS) %>%
    summarise(count, count_moe, cv, share = count/count_denom, share_moe = moe_prop(count, count_denom, count_moe, moe_denom))
  
  # # Check, compare to employed in labor force est
  # d %>% 
  #   filter(DIS == 'With a disability' & !is.na(STANDARD_JOBSECTOR)) %>% 
  #   ungroup() %>% 
  #   summarise(count = sum(count))
  
  
}

# test <- create_covempsect_pums_table(span = 1, dyear = 2022)
# test %>%
#   filter(DIS == 'With a disability' & !is.na(STANDARD_JOBSECTOR)) %>%
#   ungroup() %>%
#   summarise(count = sum(count))
