library(psrccensus)
library(psrcplot)
library(tidyverse)

# install psrccensus and get api key by going trough instructions on: https://psrc.github.io/psrccensus/articles/psrccensus.html
Sys.getenv("CENSUS_API_KEY")

# more information on PUMS data: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# 2022 5-year PUMS data dictionary: https://api.census.gov/data/2022/acs/acs5/pums/variables.html

# download 2022 5-year PUMS data with specified variables
pums_2022_h <- get_psrc_pums(span = 5,
                           dyear = 2022,
                           level = "h",
                           vars = c("AGEP",  # Age
                                    "PRACE", # Race
                                    "RAC1P", # Recoded detailed race code
                                    "RAC2P", # Recoded detailed race code
                                    "TEN",   # Tenure
                                    "GRPIP", # Gross rent as a percentage of household income past 12 months
                                    "HINCP",  # Household income
                                    "HRACE",
                                    "BIN_POVRATIO" 
                                    )) 


# full dataset
df_pums <- pums_2022_h %>%
  # make new variables
  mutate(race_aapi = case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                               PRACE == "White alone" ~ "White alone",
                               TRUE ~ PRACE),
         rent_pct_income = factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                            between(GRPIP,30,50) ~ "Between 30 and 50 percent",
                                            GRPIP > 50 ~ "Greater than 50 percent",
                                            TRUE ~ "No rent paid"),
                                  levels=c("Greater than 50 percent",
                                           "Between 30 and 50 percent",
                                           "Less than 30 percent",
                                           "No rent paid")),
         income_poverty_level = case_when(BIN_POVRATIO %in% c("under 0.50","0.50 to 0.99")~"Income below 100% of poverty level",
                                          TRUE~"Income above 100% of poverty level"))


# ---- create "RAC2P_allpersons" variable: get households with at least one AAPI member ----
pums_2022_p <- get_psrc_pums(span = 5,
                           dyear = 2022,
                           level = "p",
                           vars = c("AGEP",
                                    "TYPEHUGQ",
                                    "PRACE", # Race
                                    "RAC2P", # Recoded detailed race code
                                    "HRACE"
                           )) %>% 
  # filter only AAPI renters
  filter(AGEP >= 15,
         TYPEHUGQ == "Housing unit",
         PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) 

df_all_rac2p <- pums_2022_p[['variables']] %>%
  group_by(SERIALNO) %>%
  summarise(n_aapi = n(),
            n_prace = length(unique(PRACE)),
            n_rac2p = length(unique(RAC2P)),
            all_prace = paste(unique(PRACE),collapse = "; "),
            all_rac2p = paste(unique(RAC2P),collapse = "; ")) %>%
  ungroup() %>%
  mutate(RAC2P_allpersons = case_when(n_prace>1~"Multiple AAPI subgroups",
                                      all_prace == "Asian alone" & n_rac2p>1~"Multiple Asian alone subgroups",
                                      all_prace == "Native Hawaiian and Other Pacific Islander alone" & n_rac2p>1~"Multiple Native Hawaiian and Other Pacific Islander alone subgroups",
                                      TRUE~all_rac2p)) %>%
  select(SERIALNO,RAC2P_allpersons)

# AAPI renter dataset
df_pums_renter_aapi <- df_pums %>% 
  # filter only AAPI renters
  filter(TEN=="Rented",
         # household race assigned to household
         PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) 

df_pums_renter_aapi[['variables']] <- df_pums_renter_aapi[['variables']] %>%
  left_join(df_all_rac2p, by="SERIALNO")

# use this line code in rmarkdown notebook to read in data from this script
# source("aapi_trend2024.R")


# ---- example crosstabs ---- 
tenture <- psrc_pums_count(df_pums, group_vars=c("PRACE","TEN"))


#### replace"RAC2P" with "RAC2P_allpersons" for households with any AAPI member ####
# total number of households in each subgroup
hh_count <- psrc_pums_count(df_pums_renter_aapi, group_vars=c("PRACE","RAC2P")) %>%
  filter(RAC2P!="Total")
# share of households with income below 100% of poverty level
poverty <- psrc_pums_count(df_pums_renter_aapi, group_vars=c("RAC2P","income_poverty_level")) %>%
  filter(income_poverty_level=="Income below 100% of poverty level")
# median income + poverty level
income <- psrc_pums_median(df_pums_renter_aapi, stat_var = "HINCP", group_vars=c("RAC2P")) %>%
  left_join(poverty, by=c("DATA_YEAR","COUNTY","RAC2P"))
