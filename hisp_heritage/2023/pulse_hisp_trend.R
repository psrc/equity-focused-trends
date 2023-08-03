
library(psrcplot)
library(tidycensus)
library(psrcelmer)

library(tidyverse)
library(glue)
library(stringr)

library(odbc)
library(DBI)


library(openxlsx)
library(dplyr)
library(data.table)
library(janitor)
#where you want your outputs to go
# dest_file_loc <-
#   "C:/GitHub/equity-focused-trends/pride/census_pulse"
dest_file_loc <- "./data/"

#the same for all data tables
url_start <-
  'https://www2.census.gov/programs-surveys/demo/tables/hhp/'

#data items and their url name

#which weeks to summarize
week_range <- 46:58

sheet_name <- 'Seattle_Metro_Area'

for (week in week_range) {
  # have to assign which year each week was in to make the url
  tbls <- list('housing2')
  if (week < 52) {
    year = '2022'
  }
  else  {
    year = '2023'
  }
  week_char <- as.character(week)
  for (tbl in tbls) {
    tbl="housing2"
    url_end <-
      paste0(year,
             '/',
             'wk',
             week_char,
             '/',
             tbl,
             '_week',
             week_char,
             '.xlsx')
    url <- paste0(url_start, url_end)
    file_end <- gsub('/', '_', url_end)
    dest_file = paste0(dest_file_loc, file_end)
    
    df <- read.xlsx(url, sheet = sheet_name, fillMergedCells = TRUE)
    df_row5 <- df %>% slice(5)
    df_row35_39 <- df %>% slice(35:39)
    #df_row32_34<-df%>%slice(32:34)
    df_race <- bind_rows(df_row5, df_row35_39) %>% select(1:10)
    df_race <- df_race %>% row_to_names(row_number = 1)
    #rows 5 in the 25-30 position 32-34 position
    
    
    df_race_lng <-
      df_race %>% 
      select(-2) %>%
      pivot_longer(
        cols = c(
          'No change',
          'Rent decreased',
          'Rent increased by <$100',
          'Rent increased by <$100-$249',
          'Rent increased by <$250-$500',
          'Rent increased by more than $500',
          'Did not report',
          'Occupied without rent'
        ),
        names_to = 'rent_change_class',
        values_to = 'value'
      ) %>% 
      mutate(value = if_else(value == "-", 0.0, as.numeric(value))) %>%
      rename(select_characteristics = 'Select characteristics')  %>%
      group_by(select_characteristics) %>%
      mutate(share = value / sum(value)) %>%
      mutate(week = week)
    
    if (week == min(week_range)) {
      df_main <- df_race_lng
    } else {
      df_main <- rbind(df_main, df_race_lng)
    }
    #print(df_race_lng)
    #write.xlsx(df_lgbtq, dest_file)
    
  }
}


df_hisp <- df_main %>%
  filter(str_detect(select_characteristics, 'Hispanic or Latino')) %>%
  filter(str_detect(rent_change_class, 'more than '))
hisp_bar_chart <- interactive_column_chart(
  t=df_hisp, y='share', x='week',
  fill='select_characteristics',
  color='pognbgy_10',
  title="Share of renters with rent increase > $500"
)
hisp_bar_chart

df_totals <- df_main %>%
  group_by(select_characteristics, week) %>%
  summarize(sum(value))




