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
hh.vars <- c("HINCP","VALP","HRACE")

######################################################################################################################
# PUMS Data
######################################################################################################################
pums_data = NULL
acs_type <- 5
census_yrs <- census_5yr

for (yrs in census_yrs) {
  
  pums_hh <- get_psrc_pums(span=5, dyear=yrs, level="h", vars=hh.vars)
  
  # Median Income
  t1 <- psrc_pums_median(so=pums_hh, stat_var="HINCP", group_vars = "HRACE") %>%
    rename(name=COUNTY, year=DATA_YEAR, race=HRACE, estimate=HINCP_median, moe=HINCP_median_moe) %>%
    mutate(table="PUMS", variable="HINCP", label="Median Income", concept="Median Income", acs_type="acs5")
  
  tot <- t1 %>% filter(race=="Total") %>% select(estimate) %>% pull()
  
  t1 <- t1 %>% 
    mutate(total=tot, share=estimate/tot) %>%
    filter(race %in% c("American Indian or Alaskan Native Alone", "White alone")) %>%
    mutate(race = str_replace_all(race, "American Indian or Alaskan Native Alone", "American Indian and Alaska Native")) %>%
    mutate(race = str_replace_all(race, "White alone", "White"))
  
  # Median Home Value
  t2 <- psrc_pums_mean(so=pums_hh, stat_var="VALP", group_vars = "HRACE") %>%
    rename(name=COUNTY, year=DATA_YEAR, race=HRACE, estimate=VALP_mean, moe=VALP_mean_moe) %>%
    mutate(table="PUMS", variable="VALP", label="Median Home Value", concept="Median Home Value", acs_type="acs5")
  
  tot <- t2 %>% filter(race=="Total") %>% select(estimate) %>% pull()
  
  t2 <- t2 %>% 
    mutate(total=tot, share=estimate/tot) %>%
    filter(race %in% c("American Indian or Alaskan Native Alone", "White alone")) %>%
    mutate(race = str_replace_all(race, "American Indian or Alaskan Native Alone", "American Indian and Alaska Native")) %>%
    mutate(race = str_replace_all(race, "White alone", "White"))
  
  ifelse(is.null(pums_data), pums_data <- bind_rows(list(t1,t2)), pums_data <- bind_rows(list(pums_data, t1, t2)))
  
  rm(t1, t2, tot)
  
}

######################################################################################################################
# ACS Data
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

######################################################################################################################
# Health Outcomes
######################################################################################################################
life_expectancy <- read.csv("X:/DSA/shiny-uploads/data/life_expectancy_by_race.csv") %>%
  filter(Race %in% c("All","American Indian/Alaskan Native Only-NH","White Only-NH")) %>%
  mutate(moe = `Upper.CI` - Rate) %>%
  rename(name=Geography, year=Year, race=Race, estimate=Rate) %>%
  select(name, year, race, estimate, moe) %>%
  mutate(table="Life Expectancy", variable="LERate", label="Life Expectancy", concept="Life Expectancy", acs_type="DOH-1yr") %>%
  mutate(race = str_replace_all(race, "American Indian/Alaskan Native Only-NH", "American Indian and Alaska Native")) %>%
  mutate(race = str_replace_all(race, "White Only-NH", "White")) %>%
  mutate(name = str_replace_all(name, "State Total", "Washington")) %>%
  filter(year >=2004) %>%
  distinct()
  
tot <- life_expectancy %>% filter(race=="All") %>% select(year, estimate) %>% rename(total=estimate)

life_expectancy <- left_join(life_expectancy, tot, by=c("year")) %>% mutate(share=estimate/total) %>% filter(race!="All")
rm(tot)

######################################################################################################################
# Final Data to Output
######################################################################################################################
aian <- bind_rows(list(education,health_insurance, ownership, poverty, tribal_groupings, pums_data, life_expectancy))
write.csv(aian, paste0("aian_hertitage_data.csv"))
