library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)

#import ppe data
ppe <- read_excel("data/SNS_PPE_REPORT.xlsx")

ppe <- ppe %>% 
  clean_names() %>% 
  mutate(name = str_trim(str_to_lower(name)))

names(ppe)

#we'll remove the previously calculated fields, since we'll be calculating again with new counts
ppe <- ppe %>% 
  select(
    name,
    state_or_local,
    censuspop2010,
    cases_cdc_apr9,
    n95_respirators,
    surgical_masks,
    face_shield,
    surgical_gowns,
    coveralls,
    gloves,
    papr,
    ventilator
  )



#import terminal case counts and pare down columns
terminal_casecounts <- read_excel("data/terminal_casecounts_apr10.xlsx", 
                                        sheet = "cases")

terminal_casecounts <- terminal_casecounts %>% 
  clean_names() %>% 
  mutate(region_2 = str_trim(str_to_lower(region_2)))

names(terminal_casecounts)

term_cases <- terminal_casecounts %>% 
  select(
    name = region_2,
    term_case_counts = latest
  )

#save as RDS for later use
saveRDS(term_cases, "data/term_cases.rds")

##join 
joined_ppe <- left_join(ppe, term_cases)

#see what didn't join
joined_ppe %>% 
  filter(is.na(term_case_counts))

#the local jurisdictions hand-gathered by staff need to be incorporated

#first we'll take out the cherokee nation since no cases available in the current dataset
joined_ppe <- joined_ppe %>% 
  filter(name != "cherokee nation of oklahoma")

#then we'll create a new column to merge term number for states, hand-gathered for local
#the new column will be the one we use from here on out
joined_ppe <- joined_ppe %>% 
  mutate(
    casecount = if_else(state_or_local == "state", term_case_counts, cases_cdc_apr9)
  ) 

#now we'll use the new case count column to calculate per capita based on population
joined_ppe <- joined_ppe %>% 
  mutate(
    cases_per_100k = round_half_up(casecount / censuspop2010 * 100000)
  ) 

#finally, reorder the columns and take out ones we no longer need
joined_ppe <- joined_ppe %>% 
  select(
    name,
    state_or_local,
    casecount,
    censuspop2010,
    cases_per_100k,
    everything(),
    -cases_cdc_apr9,
    -term_case_counts
  ) 

head(joined_ppe)

#save results for next step
saveRDS(joined_ppe, "data/joined_ppe.rds")


