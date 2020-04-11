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
  arrange(desc(total_dollars)) %>% 
  View()



### FILTERING ####

#first we'll limit to just US states and DC, no territories
head(fips_codes) #built into tidycensus
us <- as_tibble(unique(fips_codes$state)[1:51])






### AGGREGATING BY STATE ####




#let's use the ppe table to piggyback off of already good-to-go state case counts and populations
tempdf <- joined_ppe %>% 
  select(
    name, state_or_local, casecount, censuspop2010, cases_per_100k
  )

#since the HHS data has only state abbreviations, not full names, we'll need to add that
#we'll use tidycensus' built in tables to pull together
statelist <- tidycensus::fips_codes %>% 
  select(state, state_name) %>% 
  distinct() %>% 
  mutate(state_name = str_to_lower(state_name))

tempdf <- left_join(tempdf, statelist, by = c("name" = "state_name")) 

tempdf

#ok now we're ready to join the temporary table back to our HHS grant data




