library(psrccensus)
library(psrcplot)
library(tidyverse)

# install psrccensus and get api key by going trough instructions on: https://psrc.github.io/psrccensus/articles/psrccensus.html
Sys.getenv("CENSUS_API_KEY")

# more information on PUMS data: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# 2022 5-year PUMS data dictionary: https://api.census.gov/data/2022/acs/acs5/pums/variables.html

# download 2022 5-year PUMS data with specified variables
pums_2022 <- get_psrc_pums(span = 5,
                           dyear = 2022,
                           level = "h",
                           vars = c("AGEP",  # Age
                                    "PRACE", # Race
                                    "RAC1P", # Recoded detailed race code
                                    "RAC2P", # Recoded detailed race code
                                    "TEN",   # Tenure
                                    "GRPIP", # Gross rent as a percentage of household income past 12 months
                                    "HINCP",  # Household income
                                    "HRACE" 
                                    )) %>% 
  # filter only AAPI renters
  filter(TEN=="Rented",
         # household race assigned to household
         PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) 


# create crosstabs
df_pums <- pums_2022 %>%
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
                                           "No rent paid")))


