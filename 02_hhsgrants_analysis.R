library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)

#load data from step 00 to use below for pulling certain columns from merged ppe for use here too
joined_ppe <- readRDS("data/joined_ppe.rds")


# HHS COVID-19 AWARD GRANT DATA ####
# source here: https://taggs.hhs.gov/coronavirus

#import the latest version of the HHS "TAGGS" data after downloading as csv
taggs_latest_raw <- read_csv("data/taggs_export_latest.csv", col_types = cols(.default = "c"))

#clean names and format columns
# **note: parse_number doesn't handle negative currency well, so because there's at least 
# one negative dollar amount in the data we'll strip out the dollar sign ($) first
taggs_latest <- taggs_latest_raw %>% 
  clean_names() %>% 
  mutate(
    award_amount = str_remove(award_amount, "\\$"), 
    award_amount = parse_number(award_amount),
    action_date = mdy(action_date)
  ) 

names(taggs_latest)


# quick counts to see what we've got here ####

#how many agencies?
taggs_latest %>% 
  group_by(opdiv) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

#how many programs?
taggs_latest %>% 
  group_by(cfda_program_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

#how many award titles?
taggs_latest %>% 
  group_by(award_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

#how many recipients?
taggs_latest %>% 
  group_by(recipient_name) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many approp codes?
taggs_latest %>% 
  group_by(approp_code) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many states? (there are territories in here too it looks like)
taggs_latest %>% 
  group_by(state) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 



### FILTERING #### 

#first we'll limit to just US states and DC, no territories
head(fips_codes) #built into tidycensus
us <- unique(fips_codes$state)[1:51]

taggs_filtered <- taggs_latest %>% 
  filter(state %in% us)

taggs_filtered %>% 
  count(state)

#per guidance from BB health team expert, we'll filter out all NIH grants
taggs_filtered <- taggs_filtered %>% 
  filter(opdiv != "NIH")

#confirm it's gone
taggs_filtered %>% 
  count(opdiv)

# **may be additional categories of grants that also need to be removed... tbd

# **also does DC itself need to be removed? Are there grants tagged as DC that have nothing to do with funding *for* DC residents?



### AGGREGATING BY STATE ####

#now let's aggregate spending by state to work with
taggs_bystate <- taggs_filtered %>% 
  group_by(state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

taggs_bystate

#let's use the ppe table to piggyback off of already good-to-go state case counts and populations
tempdf <- joined_ppe %>% 
  select(
    name, state_or_local, casecount, censuspop2010, cases_per_100k
  )

#since the HHS data has only state abbreviations, not full names, we'll need to add that
#we'll use tidycensus' built in fips table once again 
statelist <- tidycensus::fips_codes %>% 
  select(state, state_name) %>% 
  distinct() %>% 
  mutate(state_name = str_to_lower(state_name))

tempdf <- left_join(tempdf, statelist, by = c("name" = "state_name")) 

tempdf

#ok now we're ready to join the temporary table back to our HHS grant data
joined_taggs_bystate <- inner_join(tempdf, taggs_bystate, by = "state")



### CALCULATING THE RATIOS ####

#add calculated fields and ranks
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    #by population per capita
    dollars_percapita = total_dollars / censuspop2010,
    dollars_percapita_rank = min_rank(desc(dollars_percapita)),
    dollars_percase = total_dollars / casecount,
    dollars_percase_rank = min_rank(desc(dollars_percase)),
    dollars_per_casesper100kppl = total_dollars / cases_per_100k,
    dollars_per_casesper100kppl_rank = min_rank(desc(dollars_per_casesper100kppl))
  ) 

#add a ranking too for the cases/per100k itself 
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    cases_per_100k_rank = min_rank(desc(cases_per_100k))
  )

names(joined_taggs_bystate)


#save results for sharing
write_xlsx(joined_taggs_bystate, "output/joined_taggs_bystate.xlsx")


