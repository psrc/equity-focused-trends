library(psrccensus)
library(psrcplot)
library(tidyverse)

# install psrccensus and get api key by going trough instructions on: https://psrc.github.io/psrccensus/articles/psrccensus.html
Sys.getenv("CENSUS_API_KEY")

# download 2022 5-year PUMS data with specified variables
# more information on PUMS data: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# 2022 5-year PUMS data dictionary: https://api.census.gov/data/2022/acs/acs5/pums/variables.html
pums_2022 <- get_psrc_pums(span = 5,
                           dyear = 2022,
                           level = "p",
                           vars = c("AGEP","PRACE","RAC1P",
                                    "RAC2P"))
# create crosstabs
pums_age_race <- pums_2022 %>% psrc_pums_count(., group_vars=c("AGEP","PRACE"))
