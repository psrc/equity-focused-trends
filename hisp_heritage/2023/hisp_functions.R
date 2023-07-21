
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
hisp_pums_recode <- function(.pums){
  
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
                              "65 years and above")),
      veh_num = case_when(VEH == "No vehicles" ~ 0,
                          VEH == "1 vehicle" ~ 1,
                          VEH == "2 vehicles" ~ 2,
                          VEH == "3 vehicles" ~ 3,
                          VEH == "4 vehicles" ~ 4,
                          VEH == "5 vehicles" ~ 5,
                          .default = 6),
      wif_num = case_when(WIF == "No workers" ~ 0,
                          WIF == "1 worker" ~ 1,
                          WIF == "2 workers" ~ 2,
                          WIF == "3 or more workers in family" ~ 3),
      veh_wrk_ratio = ( veh_num / wif_num ),
      veh_availability = factor(case_when(veh_wrk_ratio < 1 ~ "cars < workers",
                                          veh_wrk_ratio >= 1 ~ "cars >= workers")))
}