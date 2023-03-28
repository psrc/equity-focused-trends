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
          "hhid", "sample_county", "final_home_rgcnum", "final_home_is_rgc",
          "hhsize", "vehicle_count", "hhincome_broad", "hhincome_detailed", 
          "numadults", "numchildren", "numworkers", "lifecycle",
          "res_dur", "res_type", "res_months",
          "broadband", "offpark", "offpark_cost", "streetpark")

person_vars=c("person_id",  "household_id", "gender", 
              "age", "age_category", "race_category", "race_eth_broad",
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
vars <- c("survey_year",
          "person_id",
          "household_id",
          "age",
          "gender",
          "employment",
          "jobs_count",
          "employment_pre_covid",
          "worker",
          "student",
          "schooltype",
          "school_travel",
          "school_travel_last_week",
          "education",
          "license",
          "vehicleused",
          "race_afam",
          "race_aiak",
          "race_asian",
          "race_hapi",
          "race_hisp",
          "race_white",
          "race_other",
          "race_noanswer",
          "workplace",
          "hours_work",
          "commute_freq",
          "commute_mode",
          "commute_dur",
          "telecommute_freq",
          "industry",
          "workplace_pre_covid",
          "commute_freq_pre_covid",
          "commute_mode_pre_covid",
          "telecommute_freq_pre_covid",
          "employment_change_employer",
          "employment_change_location",
          "employment_change_new_job",
          "employment_change_laid_off",
          "employment_change_left_workforce",
          "employment_change_none",
          "work_name",
          "work_county",
          "work_tract",
          "work_rgcname",
          "prev_work_wa",
          "prev_work_name",
          "prev_work_county",
          "prev_work_tract",
          "prev_work_rgcname",
          "prev_work_notwa_city",
          "prev_work_notwa_state",
          "prev_work_notwa_notus",
          "school_freq",
          "school_loc_county",
          "school_tract",
          "school_rgcname",
          "mode_freq_1",
          "mode_freq_2",
          "mode_freq_3",
          "mode_freq_4",
          "mode_freq_5",
          "tran_pass_1",
          "tran_pass_2",
          "tran_pass_3",
          "tran_pass_4",
          "tran_pass_5",
          "tran_pass_6",
          "tran_pass_7",
          "tran_pass_8",
          "tran_pass_9",
          "tran_pass_10",
          "tran_pass_11",
          "tran_pass_12",
          "benefits_1",
          "benefits_2",
          "benefits_3",
          "benefits_4")


