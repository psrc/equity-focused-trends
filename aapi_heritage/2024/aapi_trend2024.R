library(psrccensus)
library(psrcplot)
library(tidyverse)

# install psrccensus and get api key by going trough instructions on: https://psrc.github.io/psrccensus/articles/psrccensus.html
Sys.getenv("CENSUS_API_KEY")

# more information on PUMS data: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# 2022 5-year PUMS data dictionary: https://api.census.gov/data/2022/acs/acs5/pums/variables.html

# list of data
# 
# ___FULL DATASETS___
# 1. df_pums: all households 
# 2. pums_2022_p: all persons
# 
# ___AAPI HOUSEHOLD DATASETS___
# 3.df_pums_aapi: all AAPI households                                                      (use: total household count/ tenure/ income)
#   - new variables: RAC2P_aapi_group10 
#     (grouped race category: top 10 populous Asian subgroups, other Asian 
#      subgroups and NH&PI)
# 4. df_pums_renter_aapi: all AAPI renter households                                       (use: cost burden)
# 5. df_pums_aapi_allpersons: all households with any AAPI member
# 
# ___AAPI PERSONS DATASETS___
# 6. df_pums_p_aapi_renter_worker: (person-level) all adults in AAPI renters households    (use: occupation)
#    - new variables: RAC2P_aapi_group10_household (workers in RAC2P_aapi_group10 households)

# ---- full datasets ---- 

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
                                      "BIN_POVRATIO",
                                      "SOCP3")) 
## ----- 1. households ----- 
df_pums <- pums_2022_h %>%
  # make new variables
  mutate(tenure=factor(case_when(TEN=="Owned free and clear"|TEN=="Owned with mortgage or loan (include home equity loans)" ~ "owner", 
                                 TRUE ~"renter"),
                       levels=c("owner", "renter")),
         race_aapi = case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
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
         rent_pct_income_30 = factor(case_when(GRPIP < 30 ~"Less than 30 percent",
                                            GRPIP >= 30 ~ "Greater than 30 percent",
                                            TRUE ~ "No rent paid"),
                                  levels=c("Greater than 30 percent",
                                           "Less than 30 percent",
                                           "No rent paid")),
         income_poverty_level = case_when(BIN_POVRATIO %in% c("under 0.50","0.50 to 0.99")~"Income below 100% of poverty level",
                                          TRUE~"Income above 100% of poverty level"))

## ----- 2. persons ----- 
pums_2022_p <- get_psrc_pums(span = 5,
                             dyear = 2022,
                             level = "p",
                             vars = c("AGEP",
                                      "TYPEHUGQ",
                                      "PRACE",
                                      "RAC1P",
                                      "RAC2P",
                                      "SOCP3",
                                      "SOCP5",
                                      "TEN",
                                      "WAGP"
                             ))  

# ---- AAPI households data ----

# top 10 Asian subgroups with the most households
asian_top10 <- df_pums %>%
  filter(PRACE == "Asian alone") %>%
  psrc_pums_count(., group_vars=c("PRACE","RAC2P")) %>%
  filter(!RAC2P %in% c("All combinations of Asian races only","Other Asian alone","Total")) %>%
  arrange(desc(count)) %>%
  top_n(7,count)


## ----- 3. AAPI households (householder) ----- 
df_pums_aapi <- df_pums %>%
  filter(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) %>%
  mutate(
    # grouped race category: top 10 populous Asian subgroups, other Asian races and all Pacific Islander
    RAC2P_aapi_group10 = case_when(PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander",
                                   PRACE == "Asian alone" & RAC2P %in% asian_top10$RAC2P~ RAC2P,
                                   PRACE == "Asian alone"~ "Other Asian subgroups"),
    RAC2P_income = case_when(PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander",
                             RAC2P %in% c("Asian Indian alone","Cambodian alone","Chinese, except Taiwanese, alone",
                                          "Filipino alone","Japanese alone","Korean alone","Laotian alone","Pakistani alone",
                                          "Taiwanese alone","Thai alone","Vietnamese alone")~RAC2P,
                             RAC2P == "All combinations of Asian races only"~"Two or more Asian",
                             TRUE~"Other Asian"))

## ----- 4. AAPI renter households (householder) -----
df_pums_renter_aapi <- df_pums_aapi %>% filter(TEN=="Rented")

## ----- 5. AAPI households (any AAPI member in household) ----- 

# create "RAC2P_allpersons" variable: get households with at least one AAPI member
race_allpersons <- pums_2022_p[['variables']] %>% 
  # filter only AAPI adults
  filter(AGEP >= 15,
         TYPEHUGQ == "Housing unit",
         PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone")) %>%
  group_by(SERIALNO) %>%
  summarise(n_aapi = n(),
            n_prace = length(unique(PRACE)),
            n_rac2p = length(unique(RAC2P)),
            all_prace = paste(unique(PRACE),collapse = "; "),
            all_rac2p = paste(unique(RAC2P),collapse = "; ")) %>%
  ungroup() %>%
  # at least one of AAPI member in household
  mutate(PRACE_allpersons = case_when(# both Asian and PI race in household
    n_prace>1~"Multiple AAPI races",
    # at least one AAPI member in household
    all_prace=="Asian alone"~ "Asian",
    all_prace=="Native Hawaiian and Other Pacific Islander alone"~ "Native Hawaiian and Other Pacific Islander"),
    RAC2P_allpersons = case_when(# both Asian and PI race in household
      n_prace>1~"Multiple AAPI races", 
      # multiple asian subgroups in household
      all_prace == "Asian alone" & n_rac2p>1~"Multiple Asian subgroups", 
      # multiple PI subgroups in household
      all_prace == "Native Hawaiian and Other Pacific Islander alone" & n_rac2p>1~"Multiple Native Hawaiian and Other Pacific Islander subgroups",
      # at least one AAPI member in household
      all_prace=="Asian alone"~ all_rac2p,
      all_prace=="Native Hawaiian and Other Pacific Islander alone"~ all_rac2p)) %>%
  select(SERIALNO,PRACE_allpersons,RAC2P_allpersons)


df_pums_aapi_allpersons <- df_pums %>%
  filter(SERIALNO %in% race_allpersons$SERIALNO)
df_pums_aapi_allpersons[['variables']] <- df_pums_aapi_allpersons[['variables']] %>%
  left_join(race_allpersons, by="SERIALNO") %>%
  mutate(
    # grouped race category: top 10 populous Asian subgroups, other Asian races and all Pacific Islander
    RAC2P_allpersons_aapi_group10 = case_when(PRACE_allpersons == "Asian" & RAC2P_allpersons %in% asian_top10$RAC2P~ RAC2P_allpersons,
                                              PRACE_allpersons == "Asian"~ "Other Asian subgroups",
                                              TRUE~PRACE_allpersons
    ))



# ---- 6. AAPI persons data for occupation ----
# all adults in AAPI households
# possible filtering alternatives: only AAPI adults
df_pums_p_aapi_renter_worker <- pums_2022_p %>% 
  filter(AGEP >= 15,
         # PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone"),
         !is.na(SOCP3),
         SERIALNO %in% df_pums_renter_aapi[['variables']]$SERIALNO)
df_pums_p_aapi_renter_worker[['variables']] <- df_pums_p_aapi_renter_worker[['variables']] %>% 
  left_join(df_pums_renter_aapi[['variables']] %>% select(SERIALNO,PRACE,RAC2P,RAC2P_aapi_group10) %>%
              rename(RAC2P_aapi_group10_household = RAC2P_aapi_group10), 
            by="SERIALNO", suffix=c("","_houshold"))

  

# --- example crosstabs ----
# total number of households in each subgroup
hh_count <- psrc_pums_count(df_pums_renter_aapi, group_vars=c("PRACE","RAC2P")) %>%
  filter(RAC2P!="Total")
# job share for renters in entire region and each AAPI subgroup
job3_region <- pums_2022_p %>% filter(AGEP >= 15, !is.na(SOCP3), TEN=="Rented") %>%
  psrc_pums_count(., group_vars=c("SOCP3"))
job3_by_aapi_race <- psrc_pums_count(df_pums_p_aapi_renter_worker, group_vars=c("RAC2P_aapi_group10_household","SOCP3"))

# top 5 occupations of renters in entire region and each AAPI subgroup
job3_region_top_5 <- job3_region %>%
  filter(SOCP3 != "Total") %>%
  arrange(desc(share)) %>%
  top_n(5, share) %>%
  mutate(RAC2P_aapi_group10_household = "Region", .after="COUNTY")
job3_by_aapi_race_top_5 <- job3_by_aapi_race %>%
  filter(SOCP3 != "Total") %>%
  group_by(RAC2P_aapi_group10_household) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  top_n(5, share) %>%
  ungroup() %>%
  add_row(job3_region_top_5)

# income characteristics

# get all subgroups with more than 5000 population
per_count_5000 <- psrc_pums_count(df_pums_p, group_vars=c("RAC1P","RAC2P")) %>%
  filter(RAC1P=="Asian alone",
         count>5000) 

df_pums_p <- pums_2022_p %>%
  mutate(RAC2P_income = case_when(PRACE == "Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander",
                                  RAC2P == "All combinations of Asian races only"~"Two or more Asian",
                                  RAC2P == "Other Asian alone"~"Other Asian",
                                  RAC2P %in% per_count_5000$RAC2P ~RAC2P,
                                  TRUE~"Other Asian"))
per_count <- psrc_pums_count(df_pums_p, group_vars=c("RAC1P","RAC2P_income")) %>%
  filter(RAC1P=="Asian alone")
poverty <- psrc_pums_count(df_pums_aapi, group_vars=c("RAC2P_income","income_poverty_level")) %>%
  filter(income_poverty_level=="Income below 100% of poverty level")
# median income + poverty level
income <- psrc_pums_median(df_pums_aapi, stat_var = "HINCP", group_vars=c("RAC2P_income"))

test <- per_count %>%
  left_join(income, by=c("DATA_YEAR","COUNTY","RAC2P_income")) %>%
  left_join(poverty, by=c("DATA_YEAR","COUNTY","RAC2P_income"), suffix = c(".population",".below_poverty")) %>%
  filter(RAC2P_income!="Total") %>%
  mutate(RAC2P_income = str_replace(RAC2P_income," alone|, alone",""),
         RAC2P_income = factor(RAC2P_income, levels=c("Asian Indian","Cambodian","Chinese, except Taiwanese",
                                                      "Filipino","Japanese","Korean","Laotian","Pakistani",
                                                      "Taiwanese","Thai","Vietnamese","Two or more Asian","Other Asian"))) %>%
  arrange(RAC2P_income)

per_count <- psrc_pums_count(df_pums_p, group_vars=c("RAC1P")) %>%
  filter(RAC1P=="Asian alone")
poverty <- psrc_pums_count(df_pums, group_vars=c("RAC1P","income_poverty_level")) %>%
  filter(income_poverty_level=="Income below 100% of poverty level")
# median income + poverty level
income <- psrc_pums_median(df_pums, stat_var = "HINCP", group_vars=c("RAC1P"))

test2 <- per_count %>%
  left_join(income, by=c("DATA_YEAR","COUNTY","RAC1P")) %>%
  left_join(poverty, by=c("DATA_YEAR","COUNTY","RAC1P"), suffix = c(".population",".below_poverty")) %>%
  rename(RAC2P_income = RAC1P) %>%
  mutate(RAC1P = "Asian alone", .before="RAC2P_income") %>%
  add_row(test) %>%
  select(c("DATA_YEAR","RAC2P_income","count.population","count_moe.population",
           "share.below_poverty","share_moe.below_poverty","HINCP_median","HINCP_median_moe")) %>%
  mutate(`count.population` = scales::number(`count.population`,accuracy=100,big.mark = ","),
         `count_moe.population` = scales::number(`count_moe.population`,accuracy=1,big.mark = ","),
         `share.below_poverty` =  scales::percent(`share.below_poverty`,accuracy=0.1),
         `share_moe.below_poverty` =  scales::percent(`share_moe.below_poverty`,accuracy=0.1),
         HINCP_median = scales::number(HINCP_median,accuracy=100,big.mark = ","),
         HINCP_median_moe = scales::number(HINCP_median_moe,accuracy=1,big.mark = ","))

# library(openxlsx)
# write.xlsx(test2, 'income_character.xlsx')
