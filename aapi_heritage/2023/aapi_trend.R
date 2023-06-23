library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
library(psrctrends)
library(tidycensus)
library(psrcelmer)

library(tidyverse)
library(stringr)
library(rlang)
library(chron)
library(scales)
library(gridExtra)

library(odbc)
library(DBI)
library(sf)

install_psrc_fonts()

Sys.setenv(CENSUS_API_KEY = '3fc20d0d6664692c0becc323b82c752408d843d9')
Sys.getenv("CENSUS_API_KEY")



# functions
source("aapi_functions.R")

# DATA COLLECTION

# read in HHTS data
db.connect <- function(adatabase) {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\SOCKEYE",
                                database = adatabase,
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(adatabase, atable) {
  elmer_connection <- db.connect(adatabase)
  dtelm <- dbReadTable(elmer_connection, SQL(atable))
  dbDisconnect(elmer_connection)
  return(dtelm)
}

# read-in variable metadata table for levels
vars_meta <- read.dt('Elmer', 'HHSurvey.variable_metadata') %>%
  filter(table_name == "Person", survey_year==2021)

# import HHTS data ####
hh_vars=c("survey_year",
          "sample_county", "final_home_rgcnum", "final_home_is_rgc",
          "hhsize", "vehicle_count", "hhincome_broad", "hhincome_detailed", "hh_race_category",
          "numadults", "numchildren", "numworkers", "lifecycle",
          "res_dur", "res_type", "res_months",
          "broadband", "offpark", "offpark_cost", "streetpark")

person_vars=c("gender", 
              "age", "age_category", 
              "race_category", "race_eth_broad","race_hapi","race_aiak",
              "education", "workplace", "industry", "employment",
              "worker", # for calculating number of workers in the household
              
              "license",
              "commute_freq", # How often commuted to workplace last week
              "commute_mode", # Method of commuting to work location/office last week
              "telecommute_freq",
              
              "mode_freq_1", # Times ridden transit in past 30 days
              "mode_freq_2", # Times ridden a bike in past 30 days
              "mode_freq_3", # Times gone for a walk in past 30 days
              "mode_freq_4", # Times used carshare in past 30 days
              "mode_freq_5", # Times used rideshare in past 30 days
              "benefits_3" # Employer commuter benefits: Free/partially subsidized passes/fares
)

trip_vars = c("driver","mode_1","mode_simple",
              'dest_purpose_cat', 'origin_purpose_cat',
              "google_duration", 
              'trip_path_distance',
              "depart_time_hhmm", "arrival_time_hhmm", 
              "depart_time_mam", "arrival_time_mam",
              "dayofweek", "travelers_total")

hh_data_17_19<-  get_hhts("2017_2019", "h", vars=hh_vars) %>%     hh_group_data()
hh_data_17<-  get_hhts("2017", "h", vars=hh_vars) %>%     hh_group_data()
hh_data_19<-  get_hhts("2019", "h", vars=hh_vars) %>%     hh_group_data()
hh_data_21<-  get_hhts("2021", "h", vars=hh_vars) %>%     hh_group_data()

per_data_17_19<- get_hhts("2017_2019", "p", vars=person_vars) %>% per_group_data(hh_data_17_19)
per_data_17<- get_hhts("2017", "p", vars=person_vars) %>% per_group_data(hh_data_17)
per_data_19<- get_hhts("2019", "p", vars=person_vars) %>% per_group_data(hh_data_19)
per_data_21<- get_hhts("2021", "p", vars=person_vars) %>%         per_group_data(hh_data_21)

trip_data_17_19<- get_hhts("2017_2019", "t", vars=trip_vars) %>%  trip_group_data(per_data_17_19)
trip_data_21<-    get_hhts("2021", "t", vars=trip_vars) %>%       trip_group_data(per_data_21)

# ACS data ####
# missing data 
# acs_race_commute_19 <- get_acs_commute_by_race(year = 2019, type='acs1')
# acs_race_commute_21 <- get_acs_commute_by_race(year = 2021, type='acs1')

# acs_race_occupation_5year <- get_acs_occupation_by_race()

# acs_race_birthplace_5year <- get_acs_birthplace_by_race("county")
# acs_race_birthplace_1year <- get_acs_birthplace_by_race("county", type='acs1')


# PUMS data ####
# download data with all variables
pums_2019 <- get_psrc_pums(span = 1,
                           dyear = 2019,
                           level = "p",
                           vars = c("AGEP","PRACE",
                                    "RAC1P",
                                    "RAC2P",         # race with country
                                    "ANC1P",         # Ancestry - first entry
                                    "JWTRNS",        # means of transportation to work
                                    "POBP",          # Place of birth (Recode)
                                    "SOCP",          # Standard Occupational Classification (SOC) codes for 2018 and laterbased on 2018 SOC codes
                                    "VEH")) %>%      # Vehicles (1 ton or less) available
  aapi_pums_recode()
pums_2021 <- get_psrc_pums(span = 1,
                           dyear = 2021,
                           level = "p",
                           vars = c("AGEP","PRACE","JWTRNS", "HISP","RAC1P","RAC2P","ANC1P","MI_JOBSECTOR","POBP","SOCP","VEH")) %>%
  aapi_pums_recode()
pums_2017 <- get_psrc_pums(span = 1,
                           dyear = 2017,
                           level = "p",
                           vars = c("AGEP","PRACE","JWTR", "HISP","RAC1P","RAC2P","ANC1P","MI_JOBSECTOR","POBP","SOCP","VEH")) %>%
  aapi_pums_recode_17()

pums_2021_h <- get_psrc_pums(span = 1,
                           dyear = 2021,
                           level = "h",
                           vars = c("PRACE","JWTRNS", "HISP","RAC1P","MI_JOBSECTOR","POBP","SOCP","VEH")) %>%
  aapi_pums_recode()


# commute mode share by race
pums_race_commute_19_4cat <- pums_2019 %>% psrc_pums_count(., group_vars=c("race_4cat","mode_hts"))
pums_race_commute_21_4cat <- pums_2021 %>% psrc_pums_count(., group_vars=c("race_4cat","mode_hts"))
pums_race_commute_19_3cat <- pums_2019 %>% psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))
pums_race_commute_21_3cat <- pums_2021 %>% psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))

pums_race_commute_17_3cat <- pums_2017 %>% filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))

pums_country_commute_21 <- pums_2021 %>% 
  filter(race_3cat == "Asian or Pacific Islander",) %>%
  mutate(country = case_when(RAC1P=="Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                             TRUE~RAC2P)) %>%
  psrc_pums_count(., group_vars=c("RAC1P","country","mode"))



test <- pums_2021 %>% psrc_pums_count(., group_vars=c("RAC1P","PRACE"))

# remove work from home
pums_race_commute_17_no_wfm_3cat <- pums_2017 %>% 
  filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))
pums_race_commute_19_no_wfm_3cat <- pums_2019 %>% 
  filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))
pums_race_commute_21_no_wfm_3cat <- pums_2021 %>% 
  filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))

test <- pums_2019 %>% 
  psrc_pums_count(., group_vars=c("LUM_JOBSECTOR"))

# top 10 aapi group commute mode
aapi_top_10 <- pums_2019 %>% 
  filter(race_3cat == "Asian or Pacific Islander") %>%
  mutate(country = case_when(PRACE=="Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                             TRUE~RAC2P)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","country")) %>%
  filter(!country %in% c("Total","All combinations of Asian races only")) %>%
  arrange(desc(count)) %>%
  top_n(10) # identify top 10 most populous groups of aapi: aapi_top_10$country
# high telework share groups:
tele_high <- c("Asian Indian alone","Taiwanese alone","Pakistani alone","Chinese, except Taiwanese, alone","Japanese alone")

pums_race_commute_19_no_wfm_country <- pums_2019 %>% 
  mutate(country = case_when(PRACE=="Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                             TRUE~RAC2P)) %>%
  filter(race_3cat == "Asian or Pacific Islander",
         mode_hts!="Worked from home" & !is.na(mode_hts),
         country %in% aapi_top_10$country) %>%
  psrc_pums_count(., group_vars=c("race_3cat","country","mode_hts"))
pums_race_commute_21_no_wfm_country <- pums_2021 %>% 
  mutate(country = case_when(PRACE=="Native Hawaiian and Other Pacific Islander alone" ~ "Native Hawaiian and Other Pacific Islander alone",
                             TRUE~RAC2P)) %>%
  filter(race_3cat == "Asian or Pacific Islander",
         mode_hts!="Worked from home" & !is.na(mode_hts),
         country %in% aapi_top_10$country) %>%
  psrc_pums_count(., group_vars=c("race_3cat","country","mode_hts"))


# county-level
pums_race_commute_19_3cat_county <- pums_2019 %>% psrc_pums_count(., group_vars=c("COUNTY","race_3cat","mode"))
pums_race_commute_21_3cat_county <- pums_2021 %>% psrc_pums_count(., group_vars=c("COUNTY","race_3cat","mode"))
pums_race_commute_19_no_wfm_3cat_county <- pums_2019 %>% 
  filter(mode!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("COUNTY","race_3cat","mode_hts"))
pums_race_commute_21_no_wfm_3cat_county <- pums_2021 %>% 
  filter(mode!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("COUNTY","race_3cat","mode_hts"))



# vehicle ownership by race
pums_race_veh_21 <- pums_2021_h %>% 
  psrc_pums_count(., group_vars=c("race_3cat","vehicle"),
                  incl_na=FALSE)

pums_race_veh_21_per <- pums_2021 %>% 
  psrc_pums_count(., group_vars=c("race_4cat","vehicle"),
                  incl_na=FALSE)
pums_race_veh_21_county <- pums_2021_h %>% 
  psrc_pums_count(., group_vars=c("COUNTY","race_3cat","vehicle"),
                  incl_na=FALSE)

# migrate country by race
pums_race_migrate_21 <- pums_2021 %>% 
  filter(race_3cat == "Asian or Pacific Islander") %>%
  psrc_pums_count(group_vars=c("race_3cat","migrate","mode"),
                  incl_na=FALSE)

# age and race
pums_race_age_21 <- pums_2021 %>% 
  psrc_pums_count(., group_vars=c("race_3cat","age"),
                  incl_na=FALSE)



# trip analysis
hts_mode_by_race_3cat <- hhts_count(trip_data_21 %>%
                                      filter(race_eth_broad != "Child -- no race specified"),
                                    group_vars = c('race_3cat', 'mode_simple'),
                                    spec_wgt = 'trip_adult_weight_2021') %>% 
  add_row(
    hhts_count(trip_data_17_19 %>%
                 filter(race_eth_broad != "Child -- no race specified"),
               group_vars = c('race_3cat', 'mode_simple'),
               spec_wgt = 'trip_weight_2017_2019')
  ) %>% 
  filter(!is.na(mode_simple),
         mode_simple != 'Total') %>%
  mutate(survey = factor(survey, levels = c("2017_2019","2021")))

# mode and purpose share
hts_mode_purpose_by_race_3cat <- hhts_count(trip_data_21 %>%
                                      filter(race_eth_broad != "Child -- no race specified"),
                                    group_vars = c('race_3cat', "simple_purpose", 'mode_simple'),
                                    spec_wgt = 'trip_adult_weight_2021',
                                    incl_na=FALSE) %>% 
  add_row(
    hhts_count(trip_data_17_19 %>%
                 filter(race_eth_broad != "Child -- no race specified"),
               group_vars = c('race_3cat', "simple_purpose", 'mode_simple'),
               spec_wgt = 'trip_weight_2017_2019',
               incl_na=FALSE)
  ) %>% 
  filter(!is.na(mode_simple),
         mode_simple != 'Total') %>%
  mutate(survey = factor(case_when(survey=="2017_2019"~"2017/2019",
                                   TRUE~survey), levels = c("2017/2019","2021")))

# try with work/school and other trips only
test_other_than_work <- function(.data){
  .data  %>%
    filter(race_eth_broad != "Child -- no race specified") %>% 
    mutate(survey = factor(case_when(survey=="2017_2019"~"2017/2019",
                                     TRUE~survey), levels = c("2017/2019","2021")),
           purpose_work_other = case_when(simple_purpose == "Work/School"~"Work/School",
                                          TRUE ~"Other"))
    
}
hts_mode_purpose_by_race_3cat2 <- hhts_count(trip_data_21 %>%
                                               test_other_than_work(),
                                            group_vars = c('race_3cat', "purpose_work_other", 'mode_simple'),
                                            spec_wgt = 'trip_adult_weight_2021',
                                            incl_na=FALSE) %>% 
  add_row(
    hhts_count(trip_data_17_19 %>%test_other_than_work(),
               group_vars = c('race_3cat', "purpose_work_other", 'mode_simple'),
               spec_wgt = 'trip_weight_2017_2019',
               incl_na=FALSE)
  ) %>% 
  filter(mode_simple != 'Total')

hts_mode_by_race_4cat <- hhts_count(trip_data_21 %>%
                                      filter(race_eth_broad != "Child -- no race specified"),
                                    group_vars = c('race_4cat', 'mode_simple'),
                                    spec_wgt = 'trip_adult_weight_2021') %>% 
  add_row(
    hhts_count(trip_data_17_19 %>%
                 filter(race_eth_broad != "Child -- no race specified"),
               group_vars = c('race_4cat', 'mode_simple'),
               spec_wgt = 'trip_weight_2017_2019')
  ) %>% 
  filter(!is.na(mode_simple),
         mode_simple != 'Total') %>%
  mutate(survey = factor(survey, levels = c("2017_2019","2021")))

test <- hhts_count(trip_data_21 %>%
                                      filter(race_eth_broad != "Child -- no race specified"),
                                    group_vars = c("sample_county",'race_3cat', 'mode_simple'),
                                    spec_wgt = 'trip_adult_weight_2021') %>% 
  add_row(
    hhts_count(trip_data_17_19 %>%
                 filter(race_eth_broad != "Child -- no race specified"),
               group_vars = c("sample_county",'race_3cat', 'mode_simple'),
               spec_wgt = 'trip_weight_2017_2019')
  ) %>% 
  filter(!is.na(mode_simple),
         mode_simple != 'Total') %>%
  mutate(survey = factor(case_when(survey=="2017_2019"~"2017/2019",
                                   TRUE~"2021"), 
                         levels = c("2017/2019","2021")))


# to calculate total number of people
purpose_plot2 <- hhts_count(per_data_21 %>% filter(race_eth_broad != "Child -- no race specified"),
                            group_vars=c("race_aapi"),
                            spec_wgt = "person_adult_weight_2021") %>%
  filter(race_aapi != 'Total') %>%
  add_row(hhts_count(per_data_17_19 %>% 
                       filter(race_eth_broad != "Child -- no race specified"),
                     group_vars=c("race_aapi"),
                     spec_wgt = "hh_weight_2017_2019_adult") %>%
            filter(race_aapi != 'Total'))



# number of trips and total population
# total trips
purpose_plot <- hhts_count(trip_data_21 %>% filter(race_eth_broad != "Child -- no race specified") %>%
                             mutate(
                               simple_purpose2 = case_when(
                                 simple_purpose %in% c('Shop', 'Errands') ~ 'Shop/Errands',
                                 simple_purpose %in% c('Social/Recreation','Meal') ~ 'Social/Recreation/Meal',
                                 TRUE ~ simple_purpose)),
                           group_vars = c('race_aapi', 'simple_purpose'),
                           spec_wgt = "trip_adult_weight_2021",
                           incl_na=FALSE) %>%
  # drop_na(c('race_aapi', 'simple_purpose')) %>% 
  filter(simple_purpose != 'Total') %>%
  left_join(purpose_plot2, by=c("race_aapi","survey"), suffix = c('_trips', '_person')) %>% 
  mutate(
    trips_per_person = count_trips / count_person,
    moe_trips_person = moe_ratio(count_trips, count_person, count_moe_trips, count_moe_person)
  ) %>%
  add_row(
    hhts_count(
      trip_data_17_19 %>% filter(race_eth_broad != "Child -- no race specified") %>%
        mutate(
          simple_purpose2 = case_when(
            simple_purpose %in% c('Shop', 'Errands') ~ 'Shop/Errands',
            simple_purpose %in% c('Social/Recreation','Meal') ~ 'Social/Recreation/Meal',
            TRUE ~ simple_purpose)),
      group_vars = c('race_aapi', 'simple_purpose'),
      spec_wgt = "trip_weight_2017_2019",
      incl_na=FALSE) %>%
      # drop_na(c('race_aapi', 'simple_purpose')) %>% 
      filter(simple_purpose != 'Total') %>%
      left_join(purpose_plot2, by=c("race_aapi","survey"), suffix = c('_trips', '_person')) %>% 
      mutate(
        trips_per_person = count_trips / count_person,
        moe_trips_person = moe_ratio(count_trips, count_person, 
                                     count_moe_trips, count_moe_person)
      )
  )

purpose_plot$survey <- factor(purpose_plot$survey, levels = c("2017_2019","2021"))




# to calculate total number of people
trs_purpose_plot2 <- hhts_count(per_data_21 %>% filter(race_eth_broad != "Child -- no race specified"),
                            group_vars=c("race_aapi"),
                            spec_wgt = "person_adult_weight_2021") %>%
  filter(race_aapi != 'Total') %>%
  add_row(hhts_count(per_data_17_19 %>% 
                       filter(race_eth_broad != "Child -- no race specified"),
                     group_vars=c("race_aapi"),
                     spec_wgt = "hh_weight_2017_2019_adult") %>%
            filter(race_aapi != 'Total'))

# number of trips and total population
# total trips
test <- trip_data_21 %>% filter(race_eth_broad != "Child -- no race specified",
                                mode_simple %in% c("Transit"))
trs_purpose_plot <- hhts_count(trip_data_21 %>% filter(race_eth_broad != "Child -- no race specified",
                                                       mode_simple %in% c("Transit")),
                           group_vars = c('race_aapi', 'simple_purpose2'),
                           spec_wgt = "trip_adult_weight_2021",
                           incl_na=FALSE) 

# car-free households
hts_car_own <- hhts_count(hh_data_21, 
                          group_vars = c("race_3cat","vehicle_count"), 
                          spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(hh_data_17 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(hh_data_19 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2019"))

hts_car_own <- hhts_count(per_data_21, 
                          group_vars = c("race_3cat","vehicle_count"), 
                          spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(per_data_17 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(per_data_19 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2019"))
hts_car_own_detailed <- hhts_count(per_data_21, 
                          group_vars = c("race_4cat","vehicle_count"), 
                          spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(per_data_17 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_4cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(per_data_19 %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_4cat","vehicle_count"), 
                     spec_wgt = "hh_weight_2019"))

hts_commute <- hhts_count(per_data_21 %>% test(), 
                          group_vars = c("race_3cat","commute_mode3"), 
                          spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(per_data_17 %>% test() %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","commute_mode3"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(per_data_19 %>% test() %>%
                       filter(race_3cat!="Child -- no race specified"), 
                     group_vars = c("race_3cat","commute_mode3"), 
                     spec_wgt = "hh_weight_2019"))


# compare commute mode share ####

pums_commute_compare <- pums_2017 %>% 
  filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
  psrc_pums_count(., group_vars=c("race_3cat","mode_hts")) %>%
  add_row(
    pums_2019 %>% 
      filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
      psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))
  ) %>%
  add_row(
    pums_2021 %>% 
      filter(mode_hts!="Worked from home" & !is.na(mode_hts)) %>%
      psrc_pums_count(., group_vars=c("race_3cat","mode_hts"))
  )  %>%
  select(-COUNTY) %>%
  mutate(data_type="PUMS",
         DATA_YEAR = factor(as.character(DATA_YEAR), levels = c("2017","2019","2021")))

hts_commute_compare <- hhts_count(per_data_21 %>% filter(!is.na(commute_mode_compare_pums)), 
                                       group_vars = c("commute_mode_compare_pums"), 
                                       spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(per_data_17 %>% 
                       filter(race_3cat!="Child -- no race specified",
                              !is.na(commute_mode_compare_pums)), 
                     group_vars = c("commute_mode_compare_pums"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(per_data_19 %>% 
                       filter(race_3cat!="Child -- no race specified",
                              !is.na(commute_mode_compare_pums)), 
                     group_vars = c("commute_mode_compare_pums"), 
                     spec_wgt = "hh_weight_2019"))

hts_commute_race_compare <- hhts_count(per_data_21 %>% filter(!is.na(commute_mode_compare_pums)), 
                          group_vars = c("race_3cat","commute_mode_compare_pums"), 
                          spec_wgt = "person_adult_weight_2021") %>%
  add_row(hhts_count(per_data_17 %>% 
                       filter(race_3cat!="Child -- no race specified",
                              !is.na(commute_mode_compare_pums)), 
                     group_vars = c("race_3cat","commute_mode_compare_pums"), 
                     spec_wgt = "hh_weight_2017")) %>%
  add_row(hhts_count(per_data_19 %>% 
                       filter(race_3cat!="Child -- no race specified",
                              !is.na(commute_mode_compare_pums)), 
                     group_vars = c("race_3cat","commute_mode_compare_pums"), 
                     spec_wgt = "hh_weight_2019")) %>%
  select(survey:share_moe) %>%
  rename(DATA_YEAR = survey,
         mode_hts = commute_mode_compare_pums) %>%
  mutate(data_type="HTS")




test <- pums_race_commute_19_4cat %>%
  filter(!is.na(mode_hts),mode_hts!="Total") %>%
  mutate(grp = case_when(mode_hts %in% c("Drive","Other","Transit", "Walk/Bike")~"Traveling to Work",
                         mode_hts =="Worked from home"~"Telework")) %>%
  group_by(race_4cat) %>%
  mutate(total_workers = sum(count)) %>%
  group_by(DATA_YEAR,COUNTY,race_4cat,grp,total_workers) %>%
  summarise(count_workers = sum(count)) %>%
  group_by(grp) %>%
  mutate(sum_all_workers_in_grp = sum(count_workers))

test3 <- pums_commute_mode_share %>%
  filter(DATA_YEAR==2019)





