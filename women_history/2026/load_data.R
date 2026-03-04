library(tidyverse)
library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
install_psrc_fonts()

# util functions ----
percent_format <- function(.col, accuracy=0.01){
  scales::percent(.col, accuracy=accuracy)
}

# load data and data manipulation----

## hts data ----
vars <- c("survey_year", 
          "gender",
          "dest_purpose_cat",
          "dest_purpose_cat_5",
          "mode_class",
          "mode_class_5",
          "work_mode")
hts_data <- get_psrc_hts(survey_vars = vars, survey_years = c(2017,2019,2021,2023))

# gender assignment cash when formula
gender_assign <- list(
  "Female"=               c("Female",
                            "Girl/Woman (cisgender or transgender)"),
  
  "Male"=                 c("Male",
                            "Boy/Man (cisgender or transgender)"),
  
  "Non-Binary"=           c("Non-binary/Something else fits better",
                            "Non-Binary",
                            "Another"),
  
  "Prefer not to answer"= c("Prefer not to answer",
                            "Not listed here / prefer not to answer")
)
gender_cases <- imap(gender_assign, ~quo(gender %in% !!.x ~ !!.y))


# person table
df_person <- hts_data[["person"]] %>%
  mutate(
    gender_cat = case_when(!!!gender_cases),
    gender_final = ifelse(!gender_cat %in% c("Female","Male"), NA, gender_cat))

# trip table
df_trip <- hts_data[["trip"]] %>%
  mutate(
    purpose_custom = case_when(dest_purpose_cat_5 == "Home"~ NA,
                               dest_purpose_cat_5 == "Social/Recreation"~ "Social/Recreation/Meal",
                               dest_purpose_cat_5 == "Other"~ "Errand/Other",
                               dest_purpose_cat == "Personal Business/Errand/Appointment"~ "Errand/Other",
                               dest_purpose_cat == "Shopping"~ "Shop",
                               dest_purpose_cat == "Escort"~ "Escort",
                               TRUE~ dest_purpose_cat_5)
  )

# make 2017/2019 combined
df_hh_2017_2019 <- hts_data[["hh"]] %>%
  filter(survey_year %in%c("2017","2019") & hh_weight>0) %>%
  mutate(survey_year = "2017/2019",
         hh_weight = hh_weight/2)
df_person_2017_2019 <- df_person %>%
  filter(survey_year %in%c("2017","2019") & person_weight>0) %>%
  mutate(survey_year = "2017/2019",
         person_weight = person_weight/2)
df_trip_2017_2019 <- df_trip %>%
  filter(survey_year %in%c("2017","2019") & trip_weight>0) %>%
  mutate(survey_year = "2017/2019",
         trip_weight = trip_weight/2)

# final HTS data
df_hts <- hts_data
df_hts$hh <- hts_data[["hh"]] %>% 
  mutate(survey_year = as.character(survey_year)) %>% 
  add_row(df_hh_2017_2019) %>% 
  filter(!survey_year %in% c("2017","2019"))
df_hts$trip <- df_trip %>% 
  mutate(survey_year = as.character(survey_year)) %>% 
  add_row(df_trip_2017_2019) %>% 
  filter(!survey_year %in% c("2017","2019"))
df_hts$person <- df_person %>% 
  mutate(survey_year = as.character(survey_year)) %>% 
  add_row(df_person_2017_2019) %>% 
  filter(!survey_year %in% c("2017","2019"))

## PUMS data ----
# pums5_23 <- get_psrc_pums(5,2023,"p",vars=c("SEX","PRACE","RAC1P","HISP","WAGP","PERNP","WKHP","SOCP"))
# saveRDS(pums5_23, "20260225_march_blog/pums5_23.rds")
pums5_23 <- readRDS("pums5_23.rds")

rac1p_dict <- list(
  "1" = "White",
  "2" = "Black or African American",
  "3" = "American Indian or Alaskan Native",
  "4" = "American Indian or Alaskan Native",
  "5" = "American Indian or Alaskan Native",
  "6" = "Asian",
  "7" = "Native Hawaiian and Other Pacific Islander",
  "8" = "Some Other Race",
  "9" = "Two or More Races"
)
rac1p_cases <- imap(rac1p_dict, ~quo(as.character(RAC1P) %in% !!.y ~ !!.x))
sex_dict <- list(
  "1" = "Male",
  "2" = "Female"
)
sex_cases <- imap(sex_dict, ~quo(as.character(SEX) %in% !!.y ~ !!.x))

# dictionary from https://www.bls.gov/soc/socguide.htm#LINK3
SOCP_dict <- c(
  "11" = "Management",
  "13" = "Business and Financial Operations",
  "15" = "Computer and Mathematical",
  "17" = "Architecture and Engineering",
  "19" = "Life, Physical, and Social Science",
  "21" = "Community and Social Services",
  "23" = "Legal",
  "25" = "Education, Training, and Library",
  "27" = "Arts, Design, Entertainment, Sports, and Media",
  "29" = "Healthcare Practitioners and Technical",
  "31" = "Healthcare Support",
  "33" = "Protective Service",
  "35" = "Food Preparation and Serving Related",
  "37" = "Building and Grounds Cleaning and Maintenance",
  "39" = "Personal Care and Service",
  "41" = "Sales and Related",
  "43" = "Office and Administrative Support",
  "45" = "Farming, Fishing, and Forestry",
  "47" = "Construction and Extraction",
  "49" = "Installation, Maintenance, and Repair",
  "51" = "Production",
  "53" = "Transportation and Material Moving",
  "55" = "Military Specific"
)
socp_cases <- imap(SOCP_dict, ~quo(substr(as.character(SOCP),1,2) %in% !!.y ~ !!.x))

df_pums_var <- pums5_23$variables %>%
  mutate(
    # if HISP is not 1, then "Hispanic or Latino", else assign race category
    RAC1P_psrc = ifelse(HISP != 1, "Hispanic or Latino", case_when(!!!rac1p_cases)),
    SEX_psrc = case_when(!!!sex_cases),
    SEX_35hr = ifelse(WKHP>35 & PERNP>0, SEX_psrc, NA),
    PERNP_35hr = ifelse(WKHP>35 & PERNP>0, PERNP, NA),
    SOCP2 = case_when(!!!socp_cases),
    SOCP2_35hr = ifelse(WKHP>35 & PERNP>0, SOCP2, NA)
  )
df_pums <- pums5_23
df_pums$variables <- df_pums_var

# save stat for faster rendering ----
# df_income_sex_race <- psrc_pums_median(df_pums, stat_var = "PERNP_35hr", group_vars =c("SEX_psrc", "RAC1P_psrc"), incl_na = FALSE) %>%
#   filter(RAC1P_psrc!="Total") %>%
#   mutate(PERNP_35hr_format = scales::number(PERNP_35hr_median, accuracy=1000, big.mark = ",", prefix = "$"))
# saveRDS(df_income_sex_race, "20260225_march_blog/df_income_sex_race.rds")
df_income_sex_race <- readRDS("df_income_sex_race.rds")

# check sample size
# df_count_sex_race <- psrc_pums_count(df_pums, group_vars =c("SEX_psrc", "RAC1P_psrc"), incl_na = FALSE) %>%
#   filter(SEX_psrc!="Total") %>%
#   mutate(share_format = percent_format(share,1))


# df_share_sex_occupation_35hr <- psrc_pums_count(df_pums, group_vars =c("SOCP2_35hr","SEX_psrc"), incl_na = FALSE) %>%
#   filter(SEX_psrc!="Total") %>%
#   mutate(share_format = percent_format(share,1))
# get share of gender for all workers
# df_share_workers_35hr_sex <- psrc_pums_count(df_pums, group_vars =c("SEX_35hr"), incl_na = FALSE) %>%
#     filter(SEX_35hr!="Total") %>%
#     mutate(share_format = percent_format(share,1),
#            SEX_psrc = SEX_35hr,
#            SOCP2_35hr = "All Occupations") %>%
#   select(names(df_share_sex_occupation_35hr)) %>%
#   add_row(df_share_sex_occupation_35hr)
# saveRDS(df_share_workers_35hr_sex, "20260225_march_blog/df_share_workers_35hr_sex.rds")
df_share_workers_35hr_sex <- readRDS("df_share_workers_35hr_sex.rds")

# df_income_occupation <- psrc_pums_median(df_pums, stat_var = "PERNP_35hr", group_vars =c("SOCP2"), incl_na = FALSE) %>%
#     mutate(PERNP_35hr_format = scales::number(PERNP_35hr_median, accuracy=1000, big.mark = ",", prefix = "$"))
# saveRDS(df_income_occupation, "20260225_march_blog/df_income_occupation.rds")
df_income_occupation <- readRDS("df_income_occupation.rds")

# test 2021 data ----
# pums5_21 <- get_psrc_pums(5,2021,"p",vars=c("SEX","PRACE","RAC1P","HISP","WAGP","PERNP","WKHP"))
# df_pums_var <- pums5_21$variables %>%
#   mutate(
#     WAGP_35hr = ifelse(WKHP>35 & PERNP>0, WAGP, NA),
#     PERNP_35hr = ifelse(WKHP>35 & PERNP>0, PERNP, NA)
#   )
# df_pums_21 <- pums5_21
# df_pums_21$variables <- df_pums_var









