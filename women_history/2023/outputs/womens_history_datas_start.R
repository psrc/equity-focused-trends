
library(data.table)
library(leaflet)
library(shiny)
library(tidytext)
library(tidycensus)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(magrittr)
library(usethis)
library(installr)
library(sf)
library(forcats)
library(tidyr)
library(summarytools)
library(sp)
library(gridExtra)
library(ggpubr)

# packages that are from github that host functions for pulling data (do the install in R Gui, not RStudio)

#devtools::install_github("psrc/psrc.travelsurvey", force = TRUE)
#devtools::install_github("psrc/psrccensus", force = TRUE)
#devtools::install_github("psrc/psrcplot", force = TRUE)
#devtools::install_github("psrc/psrctrends", force = TRUE)

# run these after installing github changes through R Gui

library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
library(psrctrends)

install_psrc_fonts()
#setwd("C:/Coding/CURRENT_REPOS_GITHUB/document-maker/templates/equity_example/census_data")
#output_path <- "C:/Coding/CURRENT_REPOS_GITHUB/document-maker/templates/equity_example/outputs"

# for Elmer connection

library(odbc)
library(DBI)

elmer_connect <- function(){
  DBI::dbConnect(odbc::odbc(),
                 driver = "ODBC Driver 17 for SQL Server",
                 server = "AWS-PROD-SQL\\Sockeye",
                 database = "Elmer",
                 trusted_connection = "yes",
                 port = 1433)
}

elmer_connection <- elmer_connect()
# views and global variables for days and households (** the days table has been updated to include hh variables)
# surveys a,b,c, and d will be used in functions later in code

# census pulse survey data on unemployment by gender and longitudinally & clean
#census_employ <- read.csv('employ_week53.csv')
#census_employ_new <- census_employ[-c(1:3, 129:178),]
#census_employ_tidy <- census_employ_new[,-c(6:15)]
#census_renamed <- setNames(census_employ_tidy, c("characteristics","count","unemployed", "employed", "no_response"))
#census_employ_new2 <- census_renamed[-c(1:4),]
#write.csv(census_employ_new2, 'census_employ_tidy.csv')
#census_employ <- read.csv('census_employ_tidy.csv')
#census_employ <- census_employ[,-c(1, 3)]

x <- get_psrc_pums(span = 5,                       # Denoting ACS 5-year estimates; 1-year also available
                   dyear = c(2019),                 # Last data year of span
                   level = "p",                    # Unit of analysis == household ("p" used for person)
                   vars = c("ESR","SEX"))  # You can choose as many variables as you need.



y <- get_psrc_pums(span = 5,                       
                   dyear = c(2021),                 
                   level = "p",                    
                   vars = c("ESR","SEX")) 


esr_recode<- function(data, year) {
  ## rewriting labels of responses to be more concise
  temp_table <- data %>%
    mutate(ESR= as.factor(case_when(ESR == "Armed forces, at work" | 
                                      ESR == "Civilian employed, at work" |
                                      ESR == "Armed forces, with a job but not at work"  | 
                                      ESR == "Civilian employed, with a job but not at work" ~ "Employed",
                                    ESR == "Not in labor force"  | 
                                      ESR == "Unemployed" ~ "Unemployed"))) 
  temp_table
}


gender_data_19<- esr_recode(x)
gender_data_21<- esr_recode(y)


# fxns for tidying




gender_count_19 <- psrc_pums_count(x, group_vars=c("ESR", 'SEX'))
gender_count_21 <- psrc_pums_count(y, group_vars=c("ESR", 'SEX'))

gender_count_19_21<- rbind(gender_count_19, gender_count_21)

# unemployment by age and gender for 2019/2021
?static_column_chart

employ_chart <- static_column_chart(t=gender_data_test, 
                                    x = "DATA_YEAR",
                                    y = "ESR",
                                    fill = "SEX",
                                    moe = "share_moe",
                                    color="psrc_pairs",
                                    est ="percent")

employ_chart





x <- get_psrc_pums(span = 5,                       # Denoting ACS 5-year estimates; 1-year also available
                   dyear = c(2019),                 # Last data year of span
                   level = "p",                    # Unit of analysis == household ("p" used for person)
                   vars = c("ESR","SEX", "AGEP"))  # You can choose as many variables as you need.

gender_data_19 <- psrc_pums_count(x, group_vars=c("ESR","SEX", "AGEP"))

y <- get_psrc_pums(span = 5,                       
                   dyear = c(2021),                 
                   level = "p",                    
                   vars = c("ESR","SEX", "AGEP")) 

gender_data_21 <- psrc_pums_count(y, group_vars=c("ESR","SEX", "AGEP"))

gender_data_19_21 <- rbind(gender_data_19, gender_data_21) %>%
  filter(ESR != "Total")%>%
  filter(AGEP != "Total")

#unique(gender_data_19_21$ESR)
#unique(gender_data_19_21$AGE)
gender_data_19_21$AGEP <- as.numeric(as.character(gender_data_19_21$AGEP))

# fxns for tidying

smp_delivery_combo <- function(data, year) {
  ## rewriting labels of responses to be more concise
  temp_table <- data %>%
    mutate(ESR= case_when(ESR == "Armed forces, at work" | 
                            ESR == "Civilian employed, at work" |
                            ESR == "Armed forces, with a job but not at work"  | 
                            ESR == "Civilian employed, with a job but not at work" ~ "Employed",
                          ESR == "Not in labor force"  | 
                            ESR == "Unemployed" ~ "Unemployed")) %>%
    mutate(AGE = case_when(AGEP >= 18  & AGEP <= 25 ~ '18-25',
                           AGEP >= 26  & AGEP <= 35 ~ '26-35',
                           AGEP >= 36  & AGEP <= 45 ~ '36-45',
                           AGEP >= 46 ~ "46+"))
  temp_table
}


gender_data_test <- smp_delivery_combo(gender_data_19_21)


# unemployment by age and gender for 2019/2021
?static_column_chart

employ_chart <- static_column_chart(t=gender_data_test, 
                                    x = "DATA_YEAR",
                                    y = "ESR",
                                    fill = "SEX",
                                    moe = "share_moe",
                                    color="psrc_pairs",
                                    est ="percent")

employ_chart


# hhts data pull & variables

trip_vars <- c('person_id', 'numadults', 'numchildren', 'age', 'age_category', 'gender', 'employment', 
               'education', 'race_category', 'race_eth_poc','race_eth_apoc', 'commute_freq', 'commute_mode', 
               'commute_dur', 'telecommute_freq', 'telecommute_freq_pre_covid', 'mode_freq_1', 'mode_freq_2', 
               'mode_freq_3', 'mode_freq_4', 'mode_freq_5', 'num_trips_linked')

trip_info <- dbReadTable(elmer_connection, SQL("HHSurvey.v_trips"))

# survey names
survey_a <- list(survey = '2017', label = '2017')
survey_b <- list(survey = '2019', label = '2019')
survey_c <- list(survey = '2021', label = '2021')


person_survey_17 <- get_hhts(survey = survey_a$survey,level = "t", vars = trip_vars) 
person_survey_19 <- get_hhts(survey = survey_b$survey,level = "t", vars = trip_vars) 
person_survey_21 <- get_hhts(survey = survey_c$survey,level = "t", vars = trip_vars)

# removing duplicates
gender_17 <-(person_survey_17[!duplicated(person_survey_17$person_id),])
gender_19 <-(person_survey_19[!duplicated(person_survey_19$person_id),])
gender_21 <-(person_survey_21[!duplicated(person_survey_21$person_id),])

# tidy data
#tidy_person_17 <- smp_delivery_combo(person_survey_17, '2017')
#tidy_person_19 <- smp_delivery_combo(person_survey_19, '2019')
#tidy_person_21 <- smp_delivery_combo(person_survey_21, '2021')

# crosstabs for DICHOTOMOUS gender and number of trips taken by year
# unique(person_survey_17$numtrips)

num_trips_gender_17 <- hhts_count(gender_17, spec_wgt = 'trip_weight_2017', 
                                    group_vars = c('gender', 'telecommute_freq')) %>% 
  filter(telecommute_freq != 'Total') %>%
  filter(telecommute_freq != 'NA')

num_trips_gender_19 <- hhts_count(gender_19, spec_wgt = 'hh_weight_2019', 
                                  group_vars = c('gender', 'commute_freq', 'telecommute_freq')) 

num_trips_gender_21 <- hhts_count(gender_21, spec_wgt = 'combined_adult_weight', 
                                  group_vars = c('gender', 'commute_freq', 'telecommute_freq')) 

# merge dfs for 2017, 2017/2019, and 2021 - alternate approach for crosstabs

num_trips_gender_17_21 <- bind_rows(num_trips_gender_17, num_trips_gender_19, num_trips_gender_21) %>%
  mutate(period = as.factor(survey))

# crosstab for income and food delivery 

food_income_column<- static_column_chart(t= all_food_dichot_17_21,
                                         x="hhincome_dichot", y="share",
                                         f="survey",
                                         moe = "share_moe",
                                         color="psrc_pairs",
                                         est ="percent",
                                         dec=1,
                                         title="Food/Meal Deliveries by Income",
                                         subtitle="(e.g.., pizza/sushi, Grubhub)",
                                         source = "PSRC Regional Household Travel Survey")

food_income_column
