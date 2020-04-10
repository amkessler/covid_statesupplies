library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

#import cleaned dataset created in step 00
ppedata <- readRDS("data/joined.rds")

names(ppedata)


#we'll start by just looking at n95 respirators and ventilators
ppedata <- ppedata %>% 
  select(
    name, state_or_local, casecount, censuspop2010, cases_per_100k,
    n95_respirators, ventilator
  ) 


#calculate ratios based on raw population (as feds did) and per_100k (as we did)
#so that we can compare we'll add ranks, too

ppedata <- ppedata %>% 
  mutate(
    #by population per capita
    n95_percapita = n95_respirators / censuspop2010,
    n95_percapita_rank = min_rank(desc(n95_percapita)),
    vent_percapita = ventilator / censuspop2010,
    vent_percapita_rank = min_rank(desc(vent_percapita)),
    #by total cases number
    n95_bytotalcases = n95_respirators / casecount,
    n95_bytotalcases_rank = min_rank(desc(n95_bytotalcases)),
    vent_bytotalcases = ventilator / casecount,
    vent_bytotalcases_rank = min_rank(desc(vent_bytotalcases)),
    #by cases per 100k people
    n95_bycases100k = n95_respirators / cases_per_100k,
    n95_bycases100k_rank = min_rank(desc(n95_bycases100k)),
    vent_bycases100K = ventilator / cases_per_100k,
    vent_bycases100K_rank = min_rank(desc(vent_bycases100K)),
    
  ) 


#reorder columns  
names(ppedata)

ppedata_final <- ppedata %>% 
  select(
    name,
    state_or_local,
    casecount,
    censuspop2010,
    cases_per_100k,
    n95_respirators_DELIVERED = n95_respirators,
    n95_percapita_rank,
    n95_bytotalcases_rank,
    n95_bycases100k_rank,
    n95_percapita,
    n95_bycases100k,   
    n95_bytotalcases,
    ventilators_DELIVERED = ventilator,
    vent_percapita_rank,
    vent_bytotalcases_rank,
    vent_bycases100K_rank,
    vent_percapita,
    vent_bytotalcases,
    vent_bycases100K,
  )

names(ppedata_final)

#export for sharing
write_xlsx(ppedata_final, "output/ppedata_export.xlsx")


