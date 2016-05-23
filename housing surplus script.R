library(codemogAPI)
library(dplyr)
library(tidyr)



all_data_prev=county_profile(300, 1999:2009)%>%
  mutate(births=as.numeric(births),
         censusbuildingpermits=as.numeric(censusbuildingpermits),
         deaths=as.numeric(deaths),
         groupquarterspopulation=as.numeric(groupquarterspopulation),
         householdpopulation=as.numeric(householdpopulation),
         households=as.numeric(households),
         householdsize=as.numeric(householdsize),
         naturalincrease=as.numeric(naturalincrease),
         netmigration=as.numeric(netmigration),
         totalhousingunits=as.numeric(totalhousingunits),
         vacancyrate=as.numeric(vacancyrate),
         vacanthousingunits=as.numeric(vacanthousingunits)
  )


current_data=readxl::read_excel("v2015data.xlsx")%>%
  gather(variable, value, -countyfips:-county)%>%
  separate(variable, into=c("variable", "year"), sep="_")%>%
  spread(variable, value)%>%
  mutate(year=as.numeric(year))%>%
  select(countyfips, county, year, births, censusbuildingpermits=perm, deaths, groupquarterspopulation=groupQuartersPopulation, 
         householdpopulation=householdPopulation, households, householdsize=householdSize,
         naturalincrease=naturalIncrease, netmigration=netMigration, totalhousingunits=totalHousingUnits,
         vacancyrate=vacancyRate, vacanthousingunits=vacantHousingUnits)%>%
  filter(year>2009)

all_data=bind_rows(all_data_prev, current_data)

front_range=c(1,5,13,14,31,35,39,41,69,101,123)




#### Surplus Housing Ratio ####

data=all_data%>%
  select(county, countyfips, year, totalhousingunits,households, groupquarterspopulation, householdpopulation, netmigration, 
         householdsize, vacanthousingunits)%>%
  arrange(countyfips, year)%>%
  mutate(totalpopulation=groupquarterspopulation+householdpopulation,
         populationchange=totalpopulation-lag(totalpopulation),
         housingchange=totalhousingunits-lag(totalhousingunits),
         newhouseholds=households-lag(households),
         impliedHH=populationchange/householdsize,
         surplus_implied=housingchange-impliedHH,
         surplus_hh=housingchange-newhouseholds,
         surplus_supply=(housingchange+vacanthousingunits)-newhouseholds)%>%
  filter(year!=1999)
  
