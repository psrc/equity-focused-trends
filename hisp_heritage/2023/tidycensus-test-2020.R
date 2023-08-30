library(tidycensus)

demo_prof_vars_20<-load_variables(year=2020, dataset="dp")
sf_vars_10<-load_variables(year=2010, dataset='sf1')


#DP1_0092C: Total Hispanic Population

testdp <- get_decennial(geography = "tract", 
                       variables = "DP1_0092C",
                       state="WA",
                       county="King",
                       year = 2020,
                       sumfile = "dp")


testdp <- get_decennial(geography = "tract", 
                        variables = "DP1_0092C",
                        state="WA",
                        county="King",
                        year = 2020,
                        sumfile = "dp")




