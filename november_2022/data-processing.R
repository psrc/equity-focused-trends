library(psrccensus)
library(tidyverse)

######################################################################################################################
# Functions
######################################################################################################################

get_acs_summary <- function(tbl, vars, acs_type) {
  
  results <- map(census_data[[tbl]][[acs_type]], ~filter(.x, name == "Region" & variable %in% vars)) %>% 
    bind_rows() %>%
    select(name,variable,estimate,moe,label,concept,acs_type,year) %>%
    mutate(race = case_when(
      str_detect(concept,"WHITE") ~ "White",
      str_detect(concept,"INDIAN") ~ "American Indian and Alaska Native")) %>%
    mutate(concept=tbl) %>%
    separate(col=variable, sep="_", into = c("table","variable"))
  
  total <- results %>%
    filter(variable=="001") %>%
    select(table, year, estimate) %>%
    rename(total=estimate)
  
  results <- left_join(results, total, by=c("table","year")) %>%
    mutate(share=estimate/total) %>%
    filter(variable != "001")
  
  return(results)
  
}

######################################################################################################################
# General Inputs
######################################################################################################################

census_1yr <- c(2015,2021)
census_5yr <- c(2015,2020)

poverty_variables <- c("B17020C_001","B17020C_002","B17020H_001","B17020H_002")
ownership_variables <- c("B25003C_001","B25003C_002","B25003H_001","B25003H_002")
education_variables <- c("C15002C_001", "C15002C_006", "C15002C_011", "C15002H_001", "C15002H_006", "C15002H_011")
health_insurance_variables <- c("C27001C_001", "C27001C_004", "C27001C_007", "C27001C_010", "C27001H_001", "C27001H_004", "C27001H_007", "C27001H_010")

######################################################################################################################
# Census Data Downloads stored into lists
######################################################################################################################
census_data = NULL

# Tribal Groupings
acs_type <- "acs5"
census_yrs <- census_5yr
calc_by_acs <- partial(get_acs_recs, geography="county", state="Washington", counties = c("King", "Kitsap", "Pierce", "Snohomish"), table.names=c("B02014"), acs.type=acs_type)
census_data[["Tribal Groupings"]][[acs_type]] <- map(census_yrs, calc_by_acs)
census_data[["Tribal Groupings"]][[acs_type]] <- set_names(census_data[["Tribal Groupings"]][[acs_type]], census_yrs)

# Residents below the Poverty Line
acs_type <- "acs5"
census_yrs <- census_5yr
calc_by_acs <- partial(get_acs_recs, geography="county", state="Washington", counties = c("King", "Kitsap", "Pierce", "Snohomish"), table.names=c("B17020C","B17020H"), acs.type=acs_type)
census_data[["Poverty"]][[acs_type]] <- map(census_yrs, calc_by_acs)
census_data[["Poverty"]][[acs_type]] <- set_names(census_data[["Poverty"]][[acs_type]], census_yrs)

# Home Ownership
acs_type <- "acs1"
census_yrs <- census_1yr
calc_by_acs <- partial(get_acs_recs, geography="county", state="Washington", counties = c("King", "Kitsap", "Pierce", "Snohomish"), table.names=c("B25003C","B25003H"), acs.type=acs_type)
census_data[["Home Ownership"]][[acs_type]] <- map(census_yrs, calc_by_acs)
census_data[["Home Ownership"]][[acs_type]] <- set_names(census_data[["Home Ownership"]][[acs_type]], census_yrs)

# Educational Attainment
acs_type <- "acs1"
census_yrs <- census_1yr
calc_by_acs <- partial(get_acs_recs, geography="county", state="Washington", counties = c("King", "Kitsap", "Pierce", "Snohomish"), table.names=c("C15002C","C15002H"), acs.type=acs_type)
census_data[["Educational Attainment"]][[acs_type]] <- map(census_yrs, calc_by_acs)
census_data[["Educational Attainment"]][[acs_type]] <- set_names(census_data[["Educational Attainment"]][[acs_type]], census_yrs)

# Health Insurance
acs_type <- "acs1"
census_yrs <- census_1yr
calc_by_acs <- partial(get_acs_recs, geography="county", state="Washington", counties = c("King", "Kitsap", "Pierce", "Snohomish"), table.names=c("C27001C","C27001H"), acs.type=acs_type)
census_data[["Health Insurance"]][[acs_type]] <- map(census_yrs, calc_by_acs)
census_data[["Health Insurance"]][[acs_type]] <- set_names(census_data[["Health Insurance"]][[acs_type]], census_yrs)

######################################################################################################################
# Summary data to create charts with
######################################################################################################################
poverty <- get_acs_summary(tbl="Poverty", vars=poverty_variables, acs_type = "acs5")

ownership <- get_acs_summary(tbl="Home Ownership", vars=ownership_variables, acs_type = "acs1") 

education <- get_acs_summary(tbl="Educational Attainment", vars=education_variables, acs_type = "acs1") %>% 
  mutate(variable="002", label="Estimate!!Total!!Bachelor's degree or higher") %>%
  group_by(name, table, variable, label, concept, acs_type, year, race) %>%
  summarise(estimate=sum(estimate), moe=mean(moe), total=mean(total)) %>%
  mutate(share=estimate/total) %>%
  as_tibble()

health_insurance <- get_acs_summary(tbl="Health Insurance", vars=health_insurance_variables, acs_type = "acs1") %>% 
  mutate(variable="002", label="Estimate!!Total!!No health insurance coverage") %>%
  group_by(name, table, variable, label, concept, acs_type, year, race) %>%
  summarise(estimate=sum(estimate), moe=mean(moe), total=mean(total)) %>%
  mutate(share=estimate/total) %>%
  as_tibble()

acs_type = "acs5"
tribal_groupings <- map(census_data[["Tribal Groupings"]][[acs_type]], ~filter(.x, name == "Region")) %>% 
  bind_rows() %>%
  filter(!(variable %in% c("B02014_001","B02014_002","B02014_046"))) %>%
  mutate(tribal_grouping = case_when(
    estimate >= 1000 ~ label,
    estimate < 1000 ~ "American Indian Tribes with fewer than 1000 people")) %>%
  mutate(tribal_grouping = case_when(
    str_detect(label, "Two or More") ~ "Two or More American Indian and Alaska Native Tribes",
    !str_detect(label, "Two or More") ~ tribal_grouping)) %>%
  mutate(tribal_grouping = case_when(
    str_detect(label, "not specified") ~ "American Indian and Alaska Native Tribes where Tribe Not Specified",
    !str_detect(label, "not specified") ~ tribal_grouping)) %>%
  mutate(tribal_grouping = case_when(
    variable=="B02014_044" ~ "All other American Indian Tribes with only one Tribe reported",
    variable!="B02014_044"  ~ tribal_grouping)) %>%
  select(name,estimate,tribal_grouping,concept,year) %>%
  group_by(name, tribal_grouping, concept, year) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(tribal_grouping = gsub("Estimate!!Total!!Alaska Native tribes, specified!!","",tribal_grouping)) %>%
  mutate(tribal_grouping = gsub("Estimate!!Total!!American Indian tribes, specified!!","",tribal_grouping)) %>%
  mutate(tribal_grouping = gsub("Estimate!!Total:!!Alaska Native tribes, specified:!!","",tribal_grouping)) %>%
  mutate(tribal_grouping = gsub("Estimate!!Total:!!American Indian tribes, specified:!!","",tribal_grouping)) %>%
  mutate(table="B02014", variable="002", concept="Tribal Groupings", race="American Indian and Alaska Native", acs_type=acs_type, moe=NA) %>%
  rename(label=tribal_grouping)

totals <- tribal_groupings %>% select(year,estimate) %>% group_by(year) %>% summarise(total=sum(estimate))
tribal_groupings <- left_join(tribal_groupings, totals, by=c("year")) %>% mutate(share=estimate/total)
rm(totals) 

aian <- bind_rows(list(education,health_insurance, ownership, poverty, tribal_groupings))
write.csv(aian, paste0("aian_hertitage_data.csv"))
