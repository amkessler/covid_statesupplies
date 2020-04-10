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

ppedata %>% 
  mutate(
    n95_bypop = n95_respirators / censuspop2010 * 100,
    n95_bypop_rank = min_rank(n95_bypop)
  ) %>% 
  View()
  
  
  
  
  



