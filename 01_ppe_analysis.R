library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

#import cleaned dataset created in step 00
ppedata <- readRDS("data/joined.rds")

names(ppedata)

#calculate ratios based on raw population (as feds did) and per_100k (as we did)
#so that we can compare we'll add ranks, too

ppedata <- ppedata %>% 
  mutate(
    cases_per_100k_rank = min_rank(desc(cases_per_100k)),
    #by population per capita
    n95_percapita = n95_respirators / censuspop2010,
    n95_percapita_rank = min_rank(desc(n95_percapita)),
    vent_percapita = ventilator / censuspop2010,
    vent_percapita_rank = min_rank(desc(vent_percapita)),
    #by total cases number
    n95_percase = n95_respirators / casecount,
    n95_percase_rank = min_rank(desc(n95_percase)),
    vent_percase = ventilator / casecount,
    vent_percase_rank = min_rank(desc(vent_percase)),
    #by cases per 100k people
    n95_bycases100k = n95_respirators / cases_per_100k,
    n95_bycases100k_rank = min_rank(desc(n95_bycases100k)),
    vent_bycases100K = ventilator / cases_per_100k,
    vent_bycases100K_rank = min_rank(desc(vent_bycases100K)),
    
  ) 

#reorder columns and select just the n95 and ventilator measures
names(ppedata)

ppedata_n95_vent_only <- ppedata %>% 
  select(
    name,
    state_or_local,
    casecount,
    censuspop2010,
    cases_per_100k,
    cases_per_100k_rank,
    n95_respirators_DELIVERED = n95_respirators,
    n95_percapita_rank,
    n95_percase_rank,
    n95_bycases100k_rank,
    n95_percapita,
    n95_bycases100k,   
    n95_percase,
    ventilators_DELIVERED = ventilator,
    vent_percapita_rank,
    vent_percase_rank,
    vent_bycases100K_rank,
    vent_percapita,
    vent_percase,
    vent_bycases100K,
  )

names(ppedata_n95_vent_only)

#let's add a ranking for cases_per_100k itself to aid in reporting efforts and comparisons
ppedata_n95_vent_only <- ppedata_n95_vent_only %>% 
  select(name, casecount, censuspop2010, cases_per_100k, cases_per_100k_rank, 
         everything(), -state_or_local)



#export for sharing
write_xlsx(ppedata_n95_vent_only, "output/ppedata_export.xlsx")



##########################

# SECOND VARIATION -- RANKS FOR ALL PPE MATERIAL COLUMNS ####

#we'll start by taking the ppedata df from above and trimming some columns
names(ppedata)

ppedata2 <- ppedata %>% 
  select(
    !contains("bycases100k")
  ) 
  

#add the calculated columns for the other pieces of ppe equipment
#we'll aim for just per capita and per cases measures
ppedata2 <- ppedata2 %>% 
  mutate(
    surgical_masks_percapita = surgical_masks / censuspop2010,
    surgical_masks_percapita_rank = min_rank(desc(surgical_masks_percapita)),
    surgical_masks_percase = surgical_masks / casecount,
    surgical_masks_percase_rank = min_rank(desc(surgical_masks_percase)),
    
    face_shield_percapita = face_shield / censuspop2010,
    face_shield_percapita_rank = min_rank(desc(face_shield_percapita)),
    face_shield_percase = face_shield / casecount,
    face_shield_percase_rank = min_rank(desc(face_shield_percase)),
    
    surgical_gowns_percapita = surgical_gowns / censuspop2010,
    surgical_gowns_percapita_rank = min_rank(desc(surgical_gowns_percapita)),
    surgical_gowns_percase = surgical_gowns / casecount,
    surgical_gowns_percase_rank = min_rank(desc(surgical_gowns_percase)),
    
    coveralls_percapita = coveralls / censuspop2010,
    coveralls_percapita_rank = min_rank(desc(coveralls_percapita)),
    coveralls_percase = coveralls / casecount,
    coveralls_percase_rank = min_rank(desc(coveralls_percase)),
    
    gloves_percapita = gloves / censuspop2010,
    gloves_percapita_rank = min_rank(desc(gloves_percapita)),
    gloves_percase = gloves / casecount,
    gloves_percase_rank = min_rank(desc(gloves_percase)),
    
    #add one for population rank and total case count rank too here
    population_rank = min_rank(desc(censuspop2010)),
    totalcases_rank = min_rank(desc(casecount))
  )

names(ppedata2)  


#we'll select only ranking columns for our ppe categories to allow easier comparison
#will also reorder columns to group similar measures together to make visual scanning easier

ppedata2_ranksonly <- ppedata2 %>% 
  select(
    name,
    casecount,
    censuspop2010,
    population_rank,
    ends_with("percapita_rank"),
    totalcases_rank,
    ends_with("percase_rank")
  )

#great, now we've got what we need for sharing with the team

#export to excel file
write_xlsx(ppedata2_ranksonly, "output/ppedata2_ranksonly.xlsx")
