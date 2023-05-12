library(openxlsx)
library(dplyr)
library(data.table)
library(janitor)
#where you want your outputs to go
dest_file_loc <- "C:/GitHub/equity-focused-trends/pride/census_pulse"

#the same for all data tables
url_start<-'https://www2.census.gov/programs-surveys/demo/tables/hhp/'

#data items and their url name

#which weeks to summarize
week_range<-34:56

sheet_name<-'Seattle_Metro_Area'
characteristics<-'stillworkingonthispart'


for(week in week_range){
    # have to assign which year each week was in to make the url
    if(week<42){
      year='2021'
      #they changed the table names over time, ugh!!
      tbls<-list('health2a', 'health2b')
    }
    else if(week<52)
    {
      year='2022'
      # this is stupid
      if(week<49){
      tbls<-list('health2a', 'health2b')
      }
      else{
        tbls<-list('health1', 'health1')
      }
    }
    else{
      year= '2023'
      tbls<-list('health1', 'health1')
    }
    week_char<-as.character(week)
    for(tbl in tbls){
        url_end<- paste0(year, '/', 'wk', week_char, '/', tbl, '_week', week_char,'.xlsx')
        url<-paste0(url_start, url_end)
        file_end<-gsub('/', '_', url_end)
        dest_file=paste0(dest_file_loc,file_end)
        
  
        
        df<-read.xlsx(url, sheet=sheet_name,fillMergedCells = TRUE)
        df_row5<-df%>%slice(5)
        df_row25_30<-df%>%slice(25:30)
        #df_row32_34<-df%>%slice(32:34)
        df_lgbtq<-bind_rows(df_row5, df_row25_30)%>%select(1:7)
        df_lgbtq<- df_lgbtq %>%row_to_names(row_number=1)
        #rows 5 in the 25-30 position 32-34 position
        
        print(df_lgbtq)
        write.xlsx(df_lgbtq,dest_file)
        
    }
  }
  









