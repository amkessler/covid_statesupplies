library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

# In this script, we'll load and clean up two separate datasets, one and then the other
# The first is personal protective equipment (ppe) allocations sent to the states
# The second is grant data on HHS grants made to state and local governments/organizations


## 1) PPE DATA #############################


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
terminal_casecounts_apr10 <- read_excel("data/terminal_casecounts_apr10.xlsx", 
                                        sheet = "cases")

terminal_casecounts_apr10 <- terminal_casecounts_apr10 %>% 
  clean_names() %>% 
  mutate(region_2 = str_trim(str_to_lower(region_2)))

names(terminal_casecounts_apr10)

term_cases <- terminal_casecounts_apr10 %>% 
  select(
    name = region_2,
    term_case_count_apr10 = latest
  )


##join 
joined <- left_join(ppe, term_cases)

#see what didn't join
joined %>% 
  filter(is.na(term_case_count_apr10))

#the local jurisdictions hand-gathered by staff need to be incorporated

#first we'll take out the cherokee nation since no cases available in the current dataset
joined <- joined %>% 
  filter(name != "cherokee nation of oklahoma")

#then we'll create a new column to merge term number for states, hand-gathered for local
#the new column will be the one we use from here on out
joined <- joined %>% 
  mutate(
    casecount = if_else(state_or_local == "state", term_case_count_apr10, cases_cdc_apr9)
  ) 

#now we'll use the new case count column to calculate per capita based on population
joined <- joined %>% 
  mutate(
    cases_per_100k = round_half_up(casecount / censuspop2010 * 100000)
  ) 

#finally, reorder the columns and take out ones we no longer need
joined <- joined %>% 
  select(
    name,
    state_or_local,
    casecount,
    censuspop2010,
    cases_per_100k,
    everything(),
    -cases_cdc_apr9,
    -term_case_count_apr10
  ) 

head(joined)

#save results for next step
saveRDS(joined, "data/joined_ppe.rds")




#############################################################################

# 2) HHS COVID-19 AWARD GRANT DATA
# source here: https://taggs.hhs.gov/coronavirus


#import the latest version of the data after downloaded as csv
taggs_latest <- read_csv("data/taggs_export_latest.csv", col_types = cols(.default = "c"))




