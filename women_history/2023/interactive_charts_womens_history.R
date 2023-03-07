purpose_gender_chart_int<-interactive_column_chart(t= summs_2017_2019, x='simple_purpose', y='share',  fill='gender',  est='percent', title= 'Percent of Trips by  Purpose by Gender', source= 'Source: 2017/2019 PSRC Household Travel Survey',
                                                   color="psrc_pairs")

purpose_gender_chart_int

bike_gender<-interactive_column_chart(t= mode_summs_2017_2019, x='mode_simple', y='count',  fill='gender_grp', color = "psrc_pairs", est='number', title= "Number of Bike Trips by Gender", source = 'Source: 2019 PSRC Household Travel Survey Data')


bike_gender


age_sex<-interactive_column_chart(pums19_sex_age, 
                         x= 'BIN_AGE', 
                         y= 'count', 
                         fill= 'SEX',
                         est= 'number',
                         color = 'psrc_pairs', 
                         source='Source: 2019 Public Use Microsample Census data')

age_sex


telecommute <- static_facet_column_chart(t= work_loc_trend, 
                                         x='gender', 
                                         y='share',
                                         facet= 'year',
                                         fill='race_eth_poc_update', 
                                         est='percent',
                                         title = 'Increase in Telework by Gender and Race') +coord_cartesian(ylim=c(0,.45))


telecommute


transit_trend_gender_race<-static_facet_column_chart(t=trips_by_mode_transit, x= 'gender', y='share', facet= 'year', fill='race_eth_poc_update', title=
                            'Transit Shares by Race and Gender over time')+coord_cartesian(ylim=c(0, .08))


transit_trend_gender_race