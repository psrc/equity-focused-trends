library(tidyverse)
library(stringr)

# functions for data processing ####
# get ACS table by race ####
get_acs_all_race <- function(name,year,data_type){
  get_acs_recs(          geography = "county", table.names = paste(name,'A',sep=""), years = year, acs.type = data_type) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'B',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'C',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'D',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'E',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'F',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'G',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'H',sep=""), years = year, acs.type = data_type)) %>%
    add_row(get_acs_recs(geography = "county", table.names = paste(name,'I',sep=""), years = year, acs.type = data_type)) %>%
    mutate(race = case_when(grepl("WHITE ALONE",concept)                                     ~ "White alone",
                            grepl("ASIAN ALONE",concept)                                     ~ "Asian alone",
                            grepl("BLACK OR AFRICAN AMERICAN ALONE",concept)                 ~ "Black or African American alone",
                            grepl("HISPANIC OR LATINO",concept)                              ~ "Hispanic or Latino",
                            grepl("NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE",concept)~ "Native Hawaiian and other Pacific Islander alone",
                            grepl("WHITE ALONE, NOT HISPANIC OR LATINO",concept)             ~ "White alone, not Hispanic or Latino)",
                            grepl("AMERICAN INDIAN AND ALASKA NATIVE ALONE",concept)         ~ "American Indian and Alaska Native alone",
                            grepl("SOME OTHER RACE ALONE",concept)|grepl("TWO OR MORE RACES",concept)~ "Some Other Race"))
}


# PUMS: group fields ####
aapi_pums_recode <- function(.pums){
  
  .pums <- .pums %>% 
    # filter(JWTRNS!="Total", !is.na(JWTRNS)) %>%
    mutate(
      race_aapi = case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                            PRACE == "White alone" ~ "White alone",
                            TRUE ~ PRACE),
      race_3cat =  factor(case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                    PRACE == "White alone" ~ "White alone",
                                    TRUE ~ "Other people of color"),
                          levels = c("Asian or Pacific Islander","Other people of color","White alone")),
      race_4cat =  factor(case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                    PRACE == "White alone" ~ "White alone",
                                    PRACE == "Hispanic or Latino" ~ "Hispanic or Latino",
                                    PRACE == "Black or African American alone" ~ "Black or African American alone",
                                    TRUE ~ "Some Other Race(s)"),
                          levels = c("Asian or Pacific Islander","Black or African American alone","Hispanic or Latino","White alone","Some Other Race(s)")),
      mode = case_when(JWTRNS %in% c("Car, truck, or van")~"Drive",
                              JWTRNS %in% c("Bus",
                                            "Light rail, streetcar, or trolley",
                                            "Long-distance train or commuter train",
                                            "Subway or elevated rail",
                                            "Ferryboat",
                                            "Long-distance train or commuter rail")~"Public Transit",
                              JWTRNS == "Bicycle" ~"Bicycle",
                              JWTRNS == "Walked" ~"Walked",
                              JWTRNS == "Worked from home" ~"Worked from home",
                              JWTRNS %in% c("Motorcycle","Taxicab","Other method") ~"Other",
                              TRUE~JWTRNS),
      mode_hts = case_when(JWTRNS %in% c("Car, truck, or van")~"Drive",
                              JWTRNS %in% c("Bus",
                                            "Light rail, streetcar, or trolley",
                                            "Long-distance train or commuter train",
                                            "Subway or elevated rail",
                                            "Ferryboat",
                                            "Long-distance train or commuter rail")~"Transit",
                              JWTRNS %in% c("Walked","Bicycle") ~"Walk/Bike",
                              JWTRNS == "Worked from home" ~"Worked from home",
                              JWTRNS %in% c("Motorcycle","Taxicab","Other method") ~"Other",
                              TRUE~JWTRNS),
      vehicle = factor(case_when(VEH == "No vehicles"~ "No vehicle",
                                 VEH %in% c("1 vehicle",
                                            "2 vehicles",
                                            "3 vehicles",
                                            "4 vehicles",
                                            "5 vehicles",
                                            "6 or more vehicles") ~ "1+ vehicle(s)"),
                       levels = c("No vehicle","1+ vehicle(s)")),
      migrate = case_when(POBP=="Washington/WA"~"Born in state of residence",
                          grepl("/",POBP) | POBP == "Other US Island Areas, Oceania, Not Specified, or At Sea" ~"Born in other state in the United States",
                          TRUE~POBP),
      age = factor(case_when(AGEP < 18 ~ "Under 18 years",
                             AGEP < 25 ~ "18-24 years",
                             AGEP < 35 ~ "25-34 years",
                             AGEP < 45 ~ "35-44 years",
                             AGEP < 55 ~ "45-54 years",
                             AGEP < 65 ~ "55-64 years",
                             AGEP >64 ~ "65 years and above"),
                   levels = c("Under 18 years",
                              "18-24 years",
                              "25-34 years",
                              "35-44 years",
                              "45-54 years",
                              "55-64 years",
                              "65 years and above")))
}
aapi_pums_recode_17 <- function(.pums){
  
  .pums <- .pums %>% 
    # filter(JWTR!="Total", !is.na(JWTR)) %>%
    mutate(
      race_aapi = case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                            PRACE == "White alone" ~ "White alone",
                            TRUE ~ PRACE),
      race_3cat =  factor(case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                    PRACE == "White alone" ~ "White alone",
                                    TRUE ~ "Other people of color"),
                          levels = c("Asian or Pacific Islander","Other people of color","White alone")),
      race_4cat =  factor(case_when(PRACE %in% c("Asian alone","Native Hawaiian and Other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                    PRACE == "White alone" ~ "White alone",
                                    PRACE == "Hispanic or Latino" ~ "Hispanic or Latino",
                                    PRACE == "Black or African American alone" ~ "Black or African American alone",
                                    TRUE ~ "Some Other Race(s)"),
                          levels = c("Asian or Pacific Islander","Black or African American alone","Hispanic or Latino","White alone","Some Other Race(s)")),
      mode = case_when(JWTR %in% c("Car, truck, or van")~"Drive",
                              JWTR %in% c("Bus or trolley bus","Ferryboat","Railroad",
                                          "Streetcar or trolley car (carro publico in Puerto Rico)",
                                          "Subway or elevated")~"Public Transit",
                              JWTR == "Bicycle" ~"Bicycle",
                              JWTR == "Walked" ~"Walked",
                              JWTR == "Worked at home" ~"Worked from home",
                              JWTR %in% c("Motorcycle","Taxicab","Other method")~"Other",
                       TRUE~JWTR),
      mode_hts = case_when(JWTR %in% c("Car, truck, or van")~"Drive",
                                  JWTR %in% c("Bus or trolley bus","Ferryboat","Railroad",
                                                "Streetcar or trolley car (carro publico in Puerto Rico)",
                                                "Subway or elevated")~"Transit",
                                  JWTR %in% c("Walked","Bicycle") ~"Walk/Bike",
                                  JWTR == "Worked at home" ~"Worked from home",
                                  JWTR %in% c("Motorcycle","Taxicab","Other method") ~"Other",
                           TRUE~JWTR),
      vehicle = factor(case_when(VEH == "No vehicles"~ "No vehicle",
                                 VEH %in% c("1 vehicle",
                                            "2 vehicles",
                                            "3 vehicles",
                                            "4 vehicles",
                                            "5 vehicles",
                                            "6 or more vehicles") ~ "1+ vehicle(s)"),
                       levels = c("No vehicle","1+ vehicle(s)")),
      migrate = case_when(POBP=="Washington/WA"~"Born in state of residence",
                          grepl("/",POBP) | POBP == "Other US Island Areas, Oceania, Not Specified, or At Sea" ~"Born in other state in the United States",
                          TRUE~POBP),
      age = factor(case_when(AGEP < 18 ~ "Under 18 years",
                             AGEP < 25 ~ "18-24 years",
                             AGEP < 35 ~ "25-34 years",
                             AGEP < 45 ~ "35-44 years",
                             AGEP < 55 ~ "45-54 years",
                             AGEP < 65 ~ "55-64 years",
                             AGEP >64 ~ "65 years and above"),
                   levels = c("Under 18 years",
                              "18-24 years",
                              "25-34 years",
                              "35-44 years",
                              "45-54 years",
                              "55-64 years",
                              "65 years and above")))
}

rgc_hct <- c("Auburn","Bellevue","Bothell Canyon Park","Burien",
             "Everett","Federal Way","Kent","Kirkland Totem Lake",
             "Redmond-Overlake","Redmond Downtown","Renton",
             "Seattle South Lake Union","Seattle Uptown","Tukwila")
# HTS[household]: group fields ####
hh_group_data <- function(.data){
  .data <- .data %>%
    mutate(
      survey = case_when(length(unique(survey_year))>1~ "2017/2019", 
                         TRUE~as.character(survey_year)),
      vehicle_count = substring(vehicle_count,1,1),
      vehicle_count = case_when(vehicle_count==0 ~ "No vehicle", 
                                vehicle_count==1 ~ "1",
                                # vehicle_count==2 ~ "2",
                                vehicle_count %in% c(2,3,4,5,6,7,8)~ "2 or more"),
      vehicle_binary = case_when(vehicle_count=="No vehicle" ~ "No vehicle", 
                                 vehicle_count %in% c("1","2 or more")~ "1 or more"),
      hhsize = substring(hhsize,1,1),
      hhsize = case_when(hhsize %in% c(4,5,6,7,8,9)~ "4 or more",
                         TRUE ~ hhsize),
      have_child = case_when((lifecycle %in% c("Household includes children age 5-17", "Household includes children under 5")) | 
                               numchildren>0 ~ "Includes children",
                             TRUE~ "No children"),
      res_type = case_when(res_type == "Single-family house (detached house)"~ "Single-family house",
                           res_type == "Townhouse (attached house)"~ "Townhouse",
                           res_type %in% c("Building with 4 or more apartments/condos",
                                           "Building with 3 or fewer apartments/condos")~ "Apartment/Condo",
                           res_type %in% c("Other (including boat, RV, van, etc.)","Mobile home/trailer",
                                           "Dorm or institutional housing")~ "Others"),
      res_dur = case_when(res_dur %in% c("Between 2 and 3 years", "Between 3 and 5 years", 
                                         "Between 5 and 10 years","Between 10 and 20 years",
                                         "More than 20 years")~ "More than 2 years",
                          TRUE ~ res_dur),
      hhincome_broad = case_when(hhincome_broad %in% c("$100,000-$199,000",
                                                       "$200,000 or more","$100,000 or more")~"$100,000 or more",
                                 TRUE ~ hhincome_broad),
      hhincome_binary = case_when(hhincome_broad %in% c("Under $25,000","$25,000-$49,999") ~ "Under $50,000",
                                  hhincome_broad %in% c("$50,000-$74,999","$75,000-$99,999","$100,000-$199,000",
                                                        "$200,000 or more","$100,000 or more") ~ "$50,000 and over",
                                  hhincome_broad == "Prefer not to answer" ~ "Prefer not to answer"),
      hhincome_three  = case_when(hhincome_broad %in% c("Under $25,000") ~ "Under $25,000",
                                  hhincome_broad %in% c("$25,000-$49,999","$50,000-$74,999",
                                                        "$75,000-$99,999")~ "$25,000 - $99,999",
                                  hhincome_broad %in% c("$100,000-$199,000",
                                                        "$200,000 or more","$100,000 or more") ~ "$100,000 and over",
                                  TRUE ~ hhincome_broad),
      home_in_HCT = case_when(final_home_rgcnum %in% rgc_hct ~ "centers with HTC",
                              !is.na(final_home_rgcnum) ~ "other centers",
                              TRUE~ "not in centers")
    )
  
  .data$vehicle_count <- factor(.data$vehicle_count, levels=c("No vehicle","1","2 or more"))
  .data$vehicle_binary <- factor(.data$vehicle_binary, levels=c("No vehicle","1 or more"))
  .data$hhsize <- factor(.data$hhsize, levels=c("1","2","3","4 or more"))
  .data$have_child <- factor(.data$have_child, levels=c("No children","Includes children"))
  .data$res_type <- factor(.data$res_type, levels=c("Single-family house","Townhouse","Apartment/Condo","Others"))
  .data$res_dur <- factor(.data$res_dur, levels=c("Less than a year","Between 1 and 2 years","More than 2 years"))
  .data$final_home_is_rgc <- factor(.data$final_home_is_rgc, levels=c("RGC","Not RGC"))
  .data$hhincome_broad <- factor(.data$hhincome_broad, levels=c("Under $25,000","$25,000-$49,999",
                                                                "$50,000-$74,999","$75,000-$99,999",
                                                                "$100,000 or more","Prefer not to answer"))
  .data$hhincome_three <- factor(.data$hhincome_three, levels=c("Under $25,000","$25,000 - $99,999","$100,000 and over","Prefer not to answer"))
  .data$hhincome_binary <- factor(.data$hhincome_binary, levels=c("Under $50,000","$50,000 and over","Prefer not to answer"))
  .data$survey <- factor(.data$survey, levels=c("2017","2019","2017/2019","2021"))
  
  return(.data)
}

# HTS[person]: group fields ####
per_group_data <- function(.data,hh_data){
  
  .data <- .data %>%
    rename(transit_freq = mode_freq_1,
           bike_freq = mode_freq_2,
           walk_freq = mode_freq_3,
           carshare_freq = mode_freq_4,
           rideshare_freq = mode_freq_5,
           transit_pass = benefits_3) %>%
    mutate(
      race_4cat = case_when((race_eth_broad=="Other race, including multi-race non-Hispanic" & race_hapi=="Selected") | 
                              (race_eth_broad=="Asian only, non-Hispanic/Latinx") ~ "Asian or Pacific Islander",
                            # race_eth_broad=="Other race, including multi-race non-Hispanic" & race_aiak=="Selected" ~ "American Indian or Alaskan Native Alone",
                            race_eth_broad=="Black or African American only, non-Hispanic/Latinx"~"Black or African American alone",
                            race_eth_broad=="White only, non-Hispanic/Latinx"~ "White alone",
                            race_eth_broad=="Hispanic or Latinx"~"Hispanic or Latino",
                            race_eth_broad=="Other race, including multi-race non-Hispanic"~ "Other people of color",
                            TRUE~race_eth_broad),
      race_3cat =  factor(case_when(race_4cat %in% c("Asian or Pacific Islander") ~ "Asian or Pacific Islander",
                                    race_4cat == "White alone" ~ "White alone",
                                    race_4cat == "Child -- no race specified" ~ "Child -- no race specified",
                                    TRUE ~ "Other people of color"),
                          levels = c("Asian or Pacific Islander","Other people of color","White alone", "Child -- no race specified")),
      workplace_travel = case_when(workplace %in% c("Usually the same location (outside home)",
                                                    "Workplace regularly varies (different offices or jobsites)",
                                                    "Drives for a living (e.g., bus driver, salesperson)")~ "Works outside the home",
                                   workplace  %in% c("Telework some days and travel to a work location some days",
                                                     "At home (telecommute or self-employed with home office)")~ "Works at home",
                                   TRUE~workplace),
      age = case_when(age == '75-84 years' ~ '75 years or older',
                      age == '85 or years older' ~ '75 years or older',
                      TRUE ~ age),
      education2 = case_when(education=="Less than high school"|
                               education=="High school graduate"~"High school or less",
                             education=="Vocational/technical training" |
                               education=="Some college" |
                               education=="Associates degree"~"Technical or Associates",
                             education=="Bachelor degree" |
                               education=="Graduate/post-graduate degree"~"Bachelor's or higher"),
      commute_mode2 = case_when(commute_mode %in%  c("Bus (public transit)",
                                                     "Commuter rail (Sounder, Amtrak)",
                                                     "Ferry or water taxi",
                                                     "Paratransit",
                                                     "Streetcar",
                                                     "Urban rail (Link light rail, monorail)",
                                                     "Urban rail (Link light rail, monorail, streetcar)") ~ "Public transit",
                                commute_mode %in% c("Bicycle or e-bike",
                                                    "Scooter or e-scooter (e.g., Lime, Bird, Razor)")~ "Bike or micro-mobility",
                                commute_mode=="Walk, jog, or wheelchair" ~ "Walk",
                                commute_mode=="Drive alone" ~ "Drive alone",
                                commute_mode %in% c("Carpool ONLY with other household members",
                                                    "Carpool with other people not in household (may also include household members)",
                                                    "Vanpool",
                                                    "Private bus or shuttle") ~ "HOV modes",
                                is.na(commute_mode) ~ NA,
                                TRUE ~ "Other modes"),
      commute_mode3 = case_when(commute_mode %in% c("Vanpool","Private bus or shuttle","Paratransit","Ferry or water taxi",
                                                    "Bus (public transit)","Urban rail (Link light rail, monorail, streetcar)",
                                                    "Commuter rail (Sounder, Amtrak)","Streetcar","Urban rail (Link light rail, monorail)") ~"Transit",
                               commute_mode %in% c("Carpool ONLY with other household members",
                                                   "Carpool with other people not in household (may also include household members)")~"Carpool",
                               commute_mode %in% c("Bicycle or e-bike", "Walk, jog, or wheelchair")~"Walk/Bike",
                               commute_mode == "Drive alone"~"Drive Alone",
                               is.na(commute_mode) ~ NA,
                               TRUE~ "Other"),
      commute_mode_compare_pums = case_when(commute_mode %in% c("Vanpool","Private bus or shuttle","Paratransit","Ferry or water taxi",
                                                    "Bus (public transit)","Urban rail (Link light rail, monorail, streetcar)",
                                                    "Commuter rail (Sounder, Amtrak)","Streetcar","Urban rail (Link light rail, monorail)") ~"Transit",
                                commute_mode %in% c("Drive alone",
                                                    "Carpool ONLY with other household members",
                                                    "Carpool with other people not in household (may also include household members)") | 
                                  workplace == "Drives for a living (e.g., bus driver, salesperson)"~"Drive",
                                commute_mode %in% c("Bicycle or e-bike", "Walk, jog, or wheelchair")~"Walk/Bike",
                                is.na(commute_mode) ~ NA,
                                TRUE~ "Other"),
      telecommute_freq = case_when(telecommute_freq %in% c("1 day a week","2 days a week", "1-2 days")~"1-2 days",
                                   telecommute_freq %in% c("3 days a week","4 days a week", "3-4 days")~"3-4 days",
                                   telecommute_freq %in% c("5 days a week","6-7 days a week", "5+ days")~"5+ days",
                                   is.na(telecommute_freq) ~"NA",
                                   TRUE~"None"),
      telecommute_freq2 = case_when(telecommute_freq %in% c("1-2 days","3-4 days","5+ days")~"At least once a week",
                                    telecommute_freq == "NA" ~"NA",
                                    TRUE~"None"),
      transit_freq2 = case_when(transit_freq %in% c("1 day/week",
                                                    "2-4 days/week",
                                                    "5 days/week",
                                                    "6-7 days/week")~"at least 1 day/week",
                                is.na(transit_freq)~NA,
                                TRUE~ "less than 1 day/week")
    ) %>%
    # add household data
    left_join(hh_data %>%
                select(survey_year:hhincome_broad,household_id:hhincome_binary,home_in_HCT), 
              by = "household_id") 
  
  # .data$race_eth_broad <- factor(.data$race_eth_broad, 
  #                                levels=c("Asian only",
  #                                         "Black or African American only",
  #                                         "Hispanic or Latinx",
  #                                         "Native Hawaiian and Other Pacific Islander alone",
  #                                         "White only",
  #                                         "Other race, including multi-race",
  #                                         "Child -- no race specified"))
  .data$workplace <- factor(.data$workplace, 
                            levels=c("Usually the same location (outside home)",
                                     "Telework some days and travel to a work location some days",
                                     "At home (telecommute or self-employed with home office)",   
                                     "Drives for a living (e.g., bus driver, salesperson)",
                                     "Workplace regularly varies (different offices or jobsites)",                 
                                     NA ))
  .data$education2 <- factor(.data$education2, 
                             levels=c("High school or less",
                                      "Technical or Associates",
                                      "Bachelor's or higher", 
                                      NA))
  .data$education <- factor(.data$education, 
                            levels=c("Less than high school",
                                     "High school graduate",
                                     "Vocational/technical training",
                                     "Associates degree",
                                     "Some college",
                                     "Bachelor degree","Graduate/post-graduate degree", 
                                     NA))
  .data$employment <- factor(.data$employment, 
                             levels=c("Employed full time (35+ hours/week, paid)",
                                      "Employed part time (fewer than 35 hours/week, paid)",
                                      "Self-employed",
                                      "Unpaid volunteer or intern",
                                      "Homemaker",
                                      "Not currently employed",
                                      "Employed but not currently working (e.g., on leave, furloughed 100%)",
                                      "Retired",
                                      NA))
  .data$transit_pass <- factor(.data$transit_pass, levels= c("Offered, and I use",
                                                             "Offered, but I don't use",
                                                             "Not offered",
                                                             "I don't know",
                                                             NA))
  .data$commute_mode2 <- factor(.data$commute_mode2, level = c("Drive alone","HOV modes","Public transit",
                                                               "Walk","Bike or micro-mobility","Other modes","NA"))
  
  .data$commute_mode3 <- factor(.data$commute_mode3, level = c("Drive Alone","Transit","Carpool",
                                                               "Walk/Bike","Other"))
  
  .freq <- c("I never do this",
             "1 day/week",
             "2-4 days/week",
             "5 days/week",
             "6-7 days/week",
             "1-3 times in the past 30 days",
             "I do this, but not in the past 30 days",
             NA )
  
  .data$transit_freq <- factor(.data$transit_freq, levels= .freq)
  .data$bike_freq <- factor(.data$bike_freq, levels= .freq)
  .data$walk_freq <- factor(.data$walk_freq, levels= .freq)
  .data$carshare_freq <- factor(.data$carshare_freq, levels= .freq)
  .data$rideshare_freq <- factor(.data$rideshare_freq, levels= .freq)
  
  
  return(.data)
}


# HTS[trip]: group fields ####
trip_group_data <- function(.data,per_data){
  
  .data <- .data %>%
    mutate(
      mode_simple2 = case_when(mode_simple %in% c("Bike","Walk")~"Walk/Bike",
                               TRUE~mode_simple),
      simple_purpose = ifelse(dest_purpose_cat == 'Home',
                              origin_purpose_cat,dest_purpose_cat),
      simple_purpose = case_when(
        simple_purpose %in% c('Work','School', 'Work-related') ~ 'Work/School',
        simple_purpose == 'Shop' ~ 'Shop',
        simple_purpose %in% c('Escort','Errand/Other','Change mode','Home')~ 'Errands',
        is.na(simple_purpose) ~ 'Errands',
        simple_purpose %in% c('Social/Recreation','Meal') ~ 'Social/Recreation/Meal',
        TRUE ~ simple_purpose),
      simple_purpose2 = case_when(
        simple_purpose %in% c('Shop', 'Errands') ~ 'Shop/Errands',
        simple_purpose %in% c('Social/Recreation','Meal') ~ 'Social/Recreation/Meal',
        TRUE ~ simple_purpose),
      .after="mode_simple")%>%
    mutate(depart_time_mfm = case_when(substr(depart_time_hhmm,1,2)=="12" & substr(depart_time_hhmm,7,8)=="AM"~as.numeric(substr(depart_time_hhmm,4,5)),
                                       substr(depart_time_hhmm,7,8)=="AM"~as.numeric(substr(depart_time_hhmm,1,2))*60+as.numeric(substr(depart_time_hhmm,4,5)),
                                       substr(depart_time_hhmm,1,2)=="12" & substr(depart_time_hhmm,7,8)=="PM"~as.numeric(substr(depart_time_hhmm,1,2))*60+as.numeric(substr(depart_time_hhmm,4,5)),
                                       substr(depart_time_hhmm,7,8)=="PM"~(as.numeric(substr(depart_time_hhmm,1,2))+12)*60+as.numeric(substr(depart_time_hhmm,4,5))),
           
           arrival_time_mfm = case_when(substr(arrival_time_hhmm,1,2)=="12" & substr(arrival_time_hhmm,7,8)=="AM"~as.numeric(substr(arrival_time_hhmm,4,5)),
                                        substr(arrival_time_hhmm,7,8)=="AM"~as.numeric(substr(arrival_time_hhmm,1,2))*60+as.numeric(substr(arrival_time_hhmm,4,5)),
                                        substr(arrival_time_hhmm,1,2)=="12" & substr(arrival_time_hhmm,7,8)=="PM"~as.numeric(substr(arrival_time_hhmm,1,2))*60+as.numeric(substr(arrival_time_hhmm,4,5)),
                                        substr(arrival_time_hhmm,7,8)=="PM"~(as.numeric(substr(arrival_time_hhmm,1,2))+12)*60+as.numeric(substr(arrival_time_hhmm,4,5))),
           .after="arrival_time_hhmm") %>%
    mutate(travel_time = ifelse(arrival_time_mfm<depart_time_mfm,arrival_time_mfm+24*60-depart_time_mfm,arrival_time_mfm-depart_time_mfm),
           .after = "google_duration") %>%
    # add person data
    left_join(per_data %>%
                select(survey_year,person_id,sample_county:vehicle_count,vehicle_binary,
                       hhincome_broad,hhincome_binary,have_child,
                       gender,age,age_category,race_eth_broad,race_4cat,race_3cat,
                       education,education2,workplace:license,commute_mode2,commute_mode3,commute_mode_compare_pums,
                       home_in_HCT), 
              by = "person_id")
  
  .data$simple_purpose <- factor(.data$simple_purpose, 
                                 levels=c('Work/School',
                                          'Shop',
                                          'Errands',
                                          'Social/Recreation/Meal'))
  .data$simple_purpose2 <- factor(.data$simple_purpose, 
                                 levels=c('Work/School',
                                          'Shop/Errands',
                                          'Social/Recreation/Meal'))
  .data$mode_simple <- factor(.data$mode_simple, 
                              levels=c("Drive","Transit", "Bike","Walk","Other"))
  .data$mode_simple2 <- factor(.data$mode_simple2, 
                               levels=c("Drive","Transit", "Walk/Bike","Other"))
  
  return(.data)
}



## ACS[commute_by_race]: group fields ####
get_acs_commute_by_race <- function(year,type='acs5'){
  
  return(
    get_acs_all_race('B08105',year,type) %>%
      mutate(mode = case_when(label=="Estimate!!Total:"                                              ~"Total",
                              label=="Estimate!!Total:!!Car, truck, or van - drove alone"            ~"Drove alone",
                              label=="Estimate!!Total:!!Car, truck, or van - carpooled"              ~"Carpooled",
                              label=="Estimate!!Total:!!Public transportation (excluding taxicab)"   ~"Public transit",
                              label=="Estimate!!Total:!!Walked"                                      ~"Walked",
                              label=="Estimate!!Total:!!Taxicab, motorcycle, bicycle, or other means"~"Other",
                              label=="Estimate!!Total:!!Worked from home"                            ~"Work from home"),
             race_aapi = factor(case_when(race %in% c("Asian alone","Native Hawaiian and other Pacific Islander alone") ~ "Asian or Pacific Islander",
                                          race == "White alone" ~ "White alone",
                                          TRUE ~ race))) %>%
      group_by(GEOID,name,acs_type,year,race_aapi,mode) %>%
      summarise(estimate = sum(estimate),
                moe= moe_sum(moe, estimate = estimate)) %>%
      select(GEOID,name,acs_type,year,race_aapi,mode,estimate,moe) %>%
      # filter(!race %in% c("White alone, not Hispanic or Latino)","Other")) %>%
      group_by(name,race_aapi) %>%
      mutate(mode_share = estimate/estimate[mode=="Total"],
             mode_moe = moe_ratio(estimate, estimate[mode=="Total"], moe, moe[mode=="Total"]),
             mode = factor(mode, levels=c("Drove alone",
                                          "Carpooled",
                                          "Public transit",
                                          "Walked",
                                          "Other",
                                          "Work from home"))) %>%
      ungroup()
  )
  
}



## ACS[occupation_by_race]: group fields ####
get_acs_occupation_by_race <- function(){
  
  return(
    get_acs_all_race('C24010',2021,'acs5') %>%
      mutate(occupation = factor(case_when(label=="Estimate!!Total:"                                              ~"Total",
                                    label %in% c("Estimate!!Total:!!Male:!!Management, business, science, and arts occupations",
                                                 "Estimate!!Total:!!Female:!!Management, business, science, and arts occupations")          ~"Management, business, science, and arts occupations",
                                    label %in% c("Estimate!!Total:!!Male:!!Service occupations",
                                                 "Estimate!!Total:!!Female:!!Service occupations")                                          ~"Service occupations",
                                    label %in% c("Estimate!!Total:!!Male:!!Sales and office occupations",
                                                 "Estimate!!Total:!!Female:!!Sales and office occupations")                                 ~"Sales and office occupations",
                                    label %in% c("Estimate!!Total:!!Male:!!Natural resources, construction, and maintenance occupations",
                                                 "Estimate!!Total:!!Female:!!Natural resources, construction, and maintenance occupations") ~"Natural resources, construction, and maintenance occupations",
                                    label %in% c("Estimate!!Total:!!Male:!!Production, transportation, and material moving occupations",
                                                 "Estimate!!Total:!!Female:!!Production, transportation, and material moving occupations")  ~"Production, transportation, and material moving occupations"),
                                 levels = c("Total",
                                            "Management, business, science, and arts occupations",
                                            "Service occupations",
                                            "Sales and office occupations",
                                            "Natural resources, construction, and maintenance occupations","Production, transportation, and material moving occupations"))) %>%
      filter(!is.na(occupation)) %>%
      group_by(GEOID,name,acs_type,year,race,occupation) %>%
      summarise(estimate = sum(estimate),
                moe= moe_sum(moe, estimate = estimate))%>%
      select(GEOID,name,acs_type,year,race,occupation,estimate,moe) %>%
      group_by(name,race) %>%
      mutate(occupation_share = estimate/estimate[occupation=="Total"],
             occupation_moe = moe_ratio(estimate, estimate[occupation=="Total"], moe, moe[occupation=="Total"])) %>%
      ungroup()
  )
  
}


## ACS[birthplace_by_race]: group fields ####
get_acs_birthplace_by_race <- function(year,type){
  
  return(
    get_acs_all_race('B06004',year,type) %>%
      mutate(born = case_when(label=="Estimate!!Total:" ~"Total",
                              label=="Estimate!!Total:!!" ~"Born in WA state",
                              label=="Estimate!!Total:!!Born in other state in the United States" ~"Born in other state",
                              label=="Estimate!!Total:!!Native; born outside the United States" ~"Native: born outside the US",
                              label=="Estimate!!Total:!!Foreign born" ~"Foreign born")) %>%
      select(GEOID,name,acs_type,year,born,race,name,estimate,moe) %>%
      filter(!race %in% c("White alone, not Hispanic or Latino)","Other")) %>%
      group_by(name,race) %>%
      mutate(born_share = estimate/estimate[born=="Total"],
             born_moe = moe_ratio(estimate, estimate[born=="Total"], moe, moe[born=="Total"])) %>%
      ungroup() %>%
      filter(born!="Total") %>%
      mutate(born = factor(born, levels=c("Born in WA state",
                                          "Born in other state",
                                          "Native: born outside the US",
                                          "Foreign born")))
  )
  
}





# for wrapping the labels in x-axis
# scale_x_discrete(labels = label_wrap(10))
wrap_axis <- function(.data, fields, w=11){
  
  .data %>%
    mutate(cat = str_wrap({{fields}}, width=w))
}

# error bars for ggplot
moe_bars <- geom_errorbar(aes(ymin=share-share_moe, ymax=share+share_moe),
                          width=0.2, position = position_dodge(0.9))



no_moe <- function(plot){
  plot <- plot + geom_text(aes(x=.data[[x]],y=.data[[y]], 
                               label=paste0(p,prettyNum(round(.data[[y]]*fac,dec), big.mark = ","),s)),
                           check_overlap = TRUE,
                           position = ggplot2::position_dodge(0.9),
                           vjust = -0.25,
                           size = 11*0.36,
                           family="Poppins") +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line.x = element_line(color="#cbcbcb"))
  
  return(plot)
}


add_RGCs <- function(.data){
  .data <- .data %>%
    mutate(RGC = case_when(GEOID %in% rgcs_tracts_list$geoid~"RGC",
                           !GEOID %in% rgcs_tracts_list$geoid~"Not RGC"),
           urban_metro = case_when(GEOID %in% rgcs_tracts_list[rgcs_tracts_list$urban_metro=="Metro", ]$geoid~"Metro",
                                   GEOID %in% rgcs_tracts_list[rgcs_tracts_list$urban_metro=="Urban", ]$geoid~"Urban",
                                   !GEOID %in% rgcs_tracts_list$geoid~"Not RGC"))
  
  .data$RGC <- factor(.data$RGC, levels=c("RGC","Not RGC"))
  .data$urban_metro <- factor(.data$urban_metro, levels=c("Metro","Urban","Not RGC"))
  
  return(.data)
}
