library(psrccensus)
library(psrcplot)
# library(psrctrends)
library(tidycensus)
library(psrcelmer)

library(tidyverse)
library(glue)
library(stringr)
# library(rlang)
#library(chron)
# library(scales)
# library(gridExtra)

library(odbc)
library(DBI)
# library(sf)


source("./aapi_heritage/2023/aapi_functions.R")
source("./hisp_heritage/2023/hisp_functions.R")
pums_2021 <- get_psrc_pums(span = 5,
                           dyear = 2021,
                           level = "p",
                           vars = c("AGEP","PRACE",
                                    "MIG",           # lived here 1 year ago
                                    "HISP",          # detailed Hispanic origin
                                    "HISPEED",       # Has broadband internet
                                    "FACCESSP",      # Access to the Internet
                                    "HHLDRHISP",     # Detailed Hispanic origin of householder
                                    "RAC2P",         # race with country
                                    "ANC1P",         # Ancestry - first entry
                                    "JWTRNS",        # means of transportation to work
                                    "JWMNP",         # Travel time to work
                                    "NAICSP",        # NAICS recode for 2018
                                    "POBP",          # Place of birth (Recode)
                                    "GRPIP",         # Gross rent as % of household income
                                    "OWN_RENT",      # Dichotomous tenure
                                    "MI_JOBSECTOR",  # PSRC-defined manufacturing-industrial groups
                                    "SOCP",          # Standard Occupational Classification (SOC) codes for 2018 and laterbased on 2018 SOC codes
                                    "TEN",           # Tenure
                                    "VEH",           # Vehicles (1 ton or less) available
                                    "WIF"))          # Workers in family

pums_2021 <- pums_2021 %>%
  hisp_pums_recode() %>%
  mutate(ind = substr(NAICSP, 1,3))


pums_race_21 <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat"))

pums_race_commute_21_4cat <- pums_2021 %>% 
  psrc_pums_count(., group_vars=c("race_4cat","mode_hts"))

pums_race_grpip <- pums_2021 %>%
  psrc_pums_median(., stat_var="GRPIP", group_vars=c("race_4cat"))

pums_race_hispeed <- pums_2021 %>%
  filter(AGEP <= 65) %>%
  psrc_pums_count(., group_vars=c("race_4cat","HISPEED"))

pums_ages <- pums_2021 %>%
  mutate(
      age_grp = factor(case_when(AGEP < 10 ~ "Under 10 years",
                             AGEP < 20 ~ "10-19 years",
                             AGEP < 30 ~ "20-29 years",
                             AGEP < 40 ~ "30-39 years",
                             AGEP < 50 ~ "40-49 years",
                             AGEP < 60 ~ "50-59 years",
                             AGEP >= 60 ~ "60 years and above"),
                   levels = c("Under 10 years",
                              "10-19 years",
                              "20-29 years",
                              "30-39 years",
                              "40-49 years",
                              "50-59 years",
                              "60 years and above"))) %>%
  psrc_pums_count(., group_vars=c("race_4cat","age_grp"))
ages_chart <- static_bar_chart(
  t=pums_ages, y="age_grp", x="share",
  fill="race_4cat"
)
ages_chart

pums_race_vehicle <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat", "vehicle"))


# MIG by race
pums_mig <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat", "MIG")) %>%
  filter(MIG != "Total")
mig_chart <- interactive_column_chart(
  t=pums_mig, y="share", x="MIG",
  fill="race_4cat",
  color='pognbgy_10',
  moe="share_moe",
  title="mobility by race"
)
mig_chart


# Gross rent as a percentage of income
pums_race_rentpct <- pums_2021 %>%
  filter(GRPIP != "NA") %>%
  filter(OWN_RENT == 'Rented') %>%
  mutate(
    rentpct = factor(case_when(GRPIP < 25 ~ "Under 25",
                               GRPIP < 35 ~ "25-35",
                               GRPIP < 45 ~ "35-45",
                               GRPIP < 55 ~ "45-55",
                               GRPIP >= 50 ~ "55 and above"),
              levels = c("Under 25", "25-35", "35-45", "45-55", "55 and above")
      )) %>%
  psrc_pums_count(., group_vars=c("race_4cat", "rentpct")) %>%
  filter(rentpct != "Total")
rentpct_chart <-  interactive_column_chart(
    t=pums_race_rentpct,  y="share", x="rentpct",
    fill="race_4cat",
    color='pognbgy_10',
    moe="share_moe",
    title="Rent as a percentage of household income"
  )
rentpct_chart <-  interactive_column_chart(
    t=pums_race_rentpct,  y="share", x="race_4cat",
    fill="rentpct",
    color='pognbgy_10',
    moe="share_moe",
    title="Rent as a percentage of household income"
  )
rentpct_chart


# home ownership 
pums_tenure <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat","OWN_RENT")) %>%
  filter(OWN_RENT != "Total")
tenure_chart <-  interactive_column_chart(
    t=pums_tenure,  y="share", x="OWN_RENT",
    fill="race_4cat",
    color='pognbgy_10',
    moe="share_moe",
    title="Tenure by race"
  )
tenure_chart <-  interactive_column_chart(
    t=pums_tenure,  y="share", x="race_4cat",
    fill="OWN_RENT",
    color='pognbgy_10',
    moe="share_moe",
    title="Tenure by race"
  )
tenure_chart

ages_chart <- static_facet_column_chart(
  t=pums_ages, y="share", x="age_grp",
  fill="race_4cat",
  facet="race_4cat",
  color='pognbgy_10'
)
ages_chart

# travel time to work
pums_travel <- pums_2021 %>% #jwmnp (travel time to work, in minutes)
  filter(JWMNP != "NA") %>%
  mutate(
      trav_work = factor(case_when(JWMNP < 10 ~ "Under 10",
                             JWMNP < 20 ~ "10-19",
                             JWMNP < 30 ~ "20-29",
                             JWMNP < 40 ~ "30-39",
                             JWMNP < 50 ~ "40-49",
                             JWMNP < 60 ~ "50-59",
                             JWMNP < 70 ~ "60-69",
                             JWMNP < 80 ~ "70-79",
                             JWMNP >= 80 ~ "80 and above"),
                   levels = c("Under 10",
                              "10-19",
                              "20-29",
                              "30-39",
                              "40-49",
                              "50-59",
                              "60-69",
                              "70-79",
                              "80 and above"))) %>%
  filter(JWMNP < 60) %>%
  psrc_pums_count(., group_vars=c("race_4cat","trav_work"))
trav_work_chart <- interactive_column_chart(
  t=pums_travel, y="share", x="trav_work",
  fill="race_4cat",
  #facet="trav_work",
  color='pognbgy_10', 
  moe="share_moe",
  title="Travel time to work"
)
trav_work_chart

# commute mode by race
pums_mode <- pums_2021 %>%
  filter(mode_hts != "NA") %>%
  psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
  filter(mode_hts != "Total")
mode_chart <- interactive_column_chart(
  t=pums_mode, y="share", x="mode_hts",
  fill="race_4cat",
  #facet="race_4cat",
  color='pognbgy_10',
  moe="share_moe",
  title="Commute Mode choice by Race"
)
mode_chart

# commute mode by race, 1+ vehicles only
pums_mode_with_vehicles <- pums_2021 %>%
  filter(vehicle=="1+ vehicle(s)") %>%
  filter(mode_hts != "NA") %>%
  psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
  filter(mode_hts != "Total") %>%
  filter(mode_hts != 'Drive')
mode_chart_with_vehicles <- interactive_column_chart(
  t=pums_mode_with_vehicles, y="share", x="mode_hts",
  fill="race_4cat",
  #facet="race_4cat",
  color='pognbgy_10',
  moe="share_moe",
  title="Commute Mode choice by race, households with 1 or more vehicles"
)
mode_chart_with_vehicles

# commute mode by race, no vehicles
pums_mode_without_vehicles <- pums_2021 %>%
  filter(vehicle=="No vehicle") %>%
  filter(mode_hts != "NA") %>%
  psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
  filter(mode_hts != "Total")
mode_chart_with_vehicles <- interactive_column_chart(
  t=pums_mode_without_vehicles, y="share", x="mode_hts",
  fill="race_4cat",
  #facet="race_4cat",
  color='pognbgy_10',
  moe="share_moe",
  title="Commute Mode choice by race, households with no vehicles"
)
mode_chart_with_vehicles

# workers by industry by race
pums_emp <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat", "ind")) %>%
  filter(ind != "Total") %>%
  arrange(desc(share)) %>%
  arrange(race_4cat)

pums_hisp_emp <- pums_2021 %>%
  filter(race_4cat == "Hispanic or Latino") %>%
  psrc_pums_count(., group_vars=c("ind", "NAICSP")) %>%
  arrange(desc(share)) %>%
  arrange(ind)

pums_hisp_emp_detailed <- pums_2021 %>%
  filter(race_4cat == "Hispanic or Latino") %>%
  psrc_pums_count(., group_vars=c("NAICSP")) %>%
  arrange(desc(share)) 

pums_emp_mode <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat", "ind")) %>%
  filter(ind != "Total")


#commute mode choice by county
pums_mode_county <- pums_2021 %>%
  filter(mode_hts != "NA") %>%
  psrc_pums_count(., group_vars=c("COUNTY","race_4cat","mode_hts")) %>%
  filter(mode_hts != "Total")
county_mode_chart <-  static_facet_column_chart(
    t=pums_mode_county,  y="share", x="mode_hts",
    fill="race_4cat",
    color='pognbgy_10',
    facet="COUNTY",
    ncol=2,
    moe="share_moe",
    title="Commute Mode choice by County"
  )
county_mode_chart

#commute mode choice by sector, for the most common sectors among Hispanics:
  # commute mode choice: entertainment
  pums_mode_ent <- pums_2021 %>%
    filter(mode_hts != "NA") %>%
    filter(ind == "ENT") %>%
    psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
    filter(mode_hts != "Total")
  ent_mode_chart <-  interactive_column_chart(
      t=pums_mode_ent,  y="share", x="mode_hts",
      fill="race_4cat",
      color='pognbgy_5',
      moe="share_moe",
      title="Commute Mode choice: Entertainment sector"
    )
  county_mode_chart

  # commute mode choice: professional services
  pums_mode_prf <- pums_2021 %>%
    filter(mode_hts != "NA") %>%
    filter(ind == "PRF") %>%
    psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
    filter(mode_hts != "Total")
  prf_mode_chart <-  interactive_column_chart(
      t=pums_mode_prf,  y="share", x="mode_hts",
      fill="race_4cat",
      color='pognbgy_10',
      title="Commute mode choice: Professional Services sector"
    )
  prf_mode_chart

  # commute mode choice: construction
  pums_mode_con <- pums_2021 %>%
    filter(mode_hts != "NA") %>%
    filter(ind == "CON") %>%
    psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
    filter(mode_hts != "Total")
  con_mode_chart <-  interactive_column_chart(
      t=pums_mode_con,  y="share", x="mode_hts",
      fill="race_4cat",
      color='pognbgy_10',
      title="Commute Mode choice: Construction sector"
    )
  con_mode_chart
  
  # commute mode choice: retail
  pums_mode_ret <- pums_2021 %>%
    filter(mode_hts != "NA") %>%
    filter(ind == "RET") %>%
    psrc_pums_count(., group_vars=c("race_4cat","mode_hts")) %>%
    filter(mode_hts != "Total")
  ret_mode_chart <-  interactive_column_chart(
      t=pums_mode_ret,  y="share", x="mode_hts",
      fill="race_4cat",
      color='pognbgy_10',
      title="Commute Mode choice: Retail sector"
    )
  ret_mode_chart


pums_race_emp_ind <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat","ind","NAICSP")) %>%
  filter(NAICSP != "Total")
  
pums_veh <- pums_2021 %>%
  psrc_pums_count(., group_vars=c("race_4cat","VEH")) %>%
  filter(VEH != "Total")
veh_chart <-  interactive_column_chart(
    t=pums_veh,  y="share", x="VEH",
    fill="race_4cat",
    color='pognbgy_10',
    moe="share_moe"
  )
veh_chart

pums_vehicle <- pums_2021 %>%
  mutate(vehicle2 = factor(case_when(VEH == "No vehicles" ~ "No vehicle",
                              VEH == "1 vehicle" ~ "1 vehicle",
                              VEH %in% c( "2 vehicles",
                                        "3 vehicles",
                                        "4 vehicles",
                                        "5 vehicles",
                                        "6 or more vehicles") ~ "2+ vehicles"),
                   levels = c("No vehicle","1 vehicle", "2+ vehicles")) 
  )  %>%
  psrc_pums_count(., group_vars=c("race_4cat","vehicle2")) %>%
  filter(vehicle2 != "Total")
vehicle_chart <-  interactive_column_chart(
    t=pums_vehicle,  y="share", x="vehicle2",
    fill="race_4cat",
    color='pognbgy_10',
    moe="share_moe",
    title="Percent of households 0, 1 and 2+ cars"
  )
vehicle_chart

# vehicle-worker ratios
pums_vwr <- pums_2021 %>%
  filter(WIF != "No workers") %>%
  psrc_pums_count(., group_vars=c("race_4cat","veh_availability")) %>%
  filter(veh_availability != "Total")
vwr_chart <-  interactive_column_chart(
    t=pums_vwr,  y="share", x="veh_availability",
    fill="race_4cat",
    color='pognbgy_10',
    moe="share_moe",
    title="vehicle-to-worker ratio"
  )
vwr_chart


# transit use by Hispanic origin
pums_hisp_trans <- pums_2021 %>%
  filter(race_4cat == "Hispanic or Latino") %>%
  filter(mode_hts == "Transit") %>%
  psrc_pums_count(., group_vars=c("HHLDRHISP")) %>%
  mutate(hisp_type = fct_reorder(HHLDRHISP, share, .desc = TRUE)) %>%
  filter(!HHLDRHISP  %in% c("Total", "Not Spanish/Hispanic/Latino"))
hisp_trans_chart <-  interactive_bar_chart(
    t=pums_hisp_trans,  y="hisp_type", x="share",
    fill="hisp_type",
    color='pognbgy_10',
    #moe="share_moe",
    title="transit use by Hispanic origin"
  )
hisp_trans_chart
  
  

