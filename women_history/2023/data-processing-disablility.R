library(psrccensus)
library(tidyverse)
library(tidycensus)




var_list<- load_variables(2021, "acs5", cache = TRUE)



earnings_df <- get_acs_recs(geography = 'county',
                            table.names = c('B18140'),
                            years=c(2021),
                            acs.type = 'acs5')


write.csv(earnings_df,'earnings-disability.csv')



poverty_df <- get_acs_recs(geography = 'county',
                            table.names = c('C18130'),
                            years=c(2021),
                            acs.type = 'acs5')


write.csv(poverty_df, 'poverty-disability.csv')



poverty_df <- get_acs_recs(geography = 'county',
                           table.names = c('B17001'),
                           years=c(2019),
                           acs.type = 'acs5')%>% filter(label=='Estimate!!Total:!!Income in the past 12 months below poverty level:!!Male:' |label=='Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!Male:'|label=='Estimate!!Total:!!Income in the past 12 months below poverty level:!!Female:' |label=='Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!Female:')

write.csv(earnings_df, paste0(output_dir, '/poverty.csv'))



