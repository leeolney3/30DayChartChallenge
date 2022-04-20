# 30DayChartChallenge 20 New Tool
# Data: https://ec.europa.eu/eurostat/databrowser/view/GOV_10A_EXP__custom_2530850/default/table?lang=en

library(tidyverse)
library(countrycode)

df = read_csv("data/rec_sports.csv") %>% janitor::clean_names()

df1 = df %>% 
  mutate(entity=countrycode(geo, origin="iso2c", destination="country.name")) %>%
  mutate(entity=case_when(geo=="EL"~"Greece", TRUE~entity),
         obs_value=obs_value*1000000) %>% 
  select(entity,time_period, obs_value) %>%
  pivot_wider(names_from = entity, values_from=obs_value)
  
write.csv(df1, "data/rec_sports_wide.csv") #df for datawrapper input

# datawrapper visualization link: https://datawrapper.dwcdn.net/48PEV/3/
# datawrapper sharing link: https://www.datawrapper.de/_/48PEV/
# color palette from {MetBrewer}


