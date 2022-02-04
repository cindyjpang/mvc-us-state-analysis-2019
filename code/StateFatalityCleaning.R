library(readr)
library(dplyr)

HighwayFatalities <- read_csv("data/HighwayFatalities.csv")
AgeGroupFatalities <- read_csv("data/AgeGroupFatalities.csv")
StateFatalityRate <- read_csv("data/StateFatalityRate.csv")
us_census_bureau_regions_and_divisions <- read_csv("data/us census bureau regions and divisions.csv")[, c("State", "State Code", "Region")]


# merge data

StateFatalities <- merge(HighwayFatalities, 
                         us_census_bureau_regions_and_divisions, 
                         by = "State", 
                         all = TRUE)
StateFatalities <- merge(StateFatalities, 
                         AgeGroupFatalities,
                         by = "State", 
                         all = TRUE)

StateFatalities <- merge(StateFatalities, 
                         StateFatalityRate,
                         by = "State", 
                         all = TRUE)
RegionalFatalities<- StateFatalities %>%
  rename(Ages16_20 = "16 -- 20")%>%
  rename(Ages21_24 = "21 -- 24")%>%
  mutate(teen_fatalities = Ages16_20 + Ages21_24)%>%
  group_by(Region)%>%
  summarize(region_total_fatalities = sum(Total),
            region_teen_fatalities = sum(teen_fatalities),
            region_rural_fatalities = sum(Rural),
            region_avg_FR = mean(`2019_FR_per_100M`))%>%
  mutate(teen_fatality_prev = region_teen_fatalities/region_total_fatalities, 
         rural_fatality_prev = region_rural_fatalities/region_total_fatalities)
write.csv(RegionalFatalities, "data/RegionalFatalitySummary.csv")
  
  